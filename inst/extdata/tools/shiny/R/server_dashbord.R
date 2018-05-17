source("config.R")
convertkb2size <- function (size) {
  mb <- 1024
  gb <- 1024*1024
  tb <- 1024*1024*1024
  pb <- 1024*1024*1024*1024
  eb <- 1024*1024*1024*1024*1024
  zb <- 1024*1024*1024*1024*1024*1024
  yb <- 1024*1024*1024*1024*1024*1024*1024

  if(size < mb){
    return(paste0(round(size/kb,2), "KB"))
  } else if(size < gb){
    return(paste0(round(size/mb,2), "MB"))
  } else if(size < tb){
    return(paste0(round(size/gb,2), "GB"))
  } else if(size < pb){
    return(paste0(round(size/tb,2), "TB"))
  } else if(size < eb){
    return(paste0(round(size/pb,2), "PB"))
  } else if(size < zb){
    return(paste0(round(size/eb,2), "EB"))
  } else if(size < yb){
    return(paste0(round(size/zb,2), "ZB"))
  }
}

disk.usage <- function(path = Sys.getenv("HOME")) {
  if(length(system("which df", intern = TRUE))) {
    cmd <- sprintf("df %s", path)
    exec <- system(cmd, intern = TRUE)
    exec <- strsplit(exec[length(exec)], "[ ]+")[[1]]
    exec <- as.numeric(exec[2:4])
    structure(exec, names = c("total", "used", "available"))
  } else {
    print("'df' command not found")
  }
}

set_monitor_progress_bar <- function(id, ratio, span_text) {
  sprintf('$("#%s").attr("aria-valuenow", 100);
                 $("#%s").css("width", "%s%%");
          document.getElementById("%s-text").innerHTML= "%s";',
          id, id, ratio, id, span_text)
}

custom_render_DT <- function(output, input_id, cmd = NULL, func = NULL, caption = "") {
    output[[input_id]] <- DT::renderDataTable({
      if (!is.null(cmd)){
        eval(parse(text = cmd))
      } else if (!is.null(func)) {
        func
      }
    }, rownames = FALSE, editable = FALSE, caption = caption,
    escape = FALSE, extensions = c("Buttons", "FixedColumns", "Scroller"),
    options = list(autoWidth = TRUE, dom = "Bfrtlip", deferRender = TRUE,
                   searchHighlight = TRUE, scrollX = TRUE, lengthMenu = list(list(5,
                   10, 25, 50, -1), list(5, 10, 25, 50, "All")), buttons = c("copy",
                   "csv", "excel", "pdf", "print"), columnDefs = list(list(width = "100px",
                   targets = "_all")), initComplete = DT::JS("function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': '#487ea5', 'color': '#fff'});",
                   "}")), selection = "none")
  return(output)
  }

update_system_monitor <- function(input, output) {
  output$system_monitor <- renderUI({
    # disk monitor
    disk.usage.values <- disk.usage()
    disk.used <- disk.usage.values[2]
    if (stringr::str_detect(version$os, 'darwin')){
      disk.total <- disk.usage.values[3]
    } else {
      disk.total <- disk.usage.values[1]
    }
    ratio <- round(disk.used/disk.total, 2) * 100
    html_text <- paste0(readLines("./www/system_monitor.html"), collapse = "\n")
    span_text <- sprintf("%s%% (%s/%s)", ratio, convertkb2size(disk.used),
                         convertkb2size(disk.total))

    html_text <- sprintf('%s\n<script>%s',
                         html_text,
                         set_monitor_progress_bar("diskmonitor", ratio, span_text))

    # memory monitor

    memory.info <- as.data.frame(gc())
    total_ram <- as.numeric(stringr::str_extract(benchmarkme::get_ram(), "[0-9.]*"))
    total_ram <- total_ram / 1024
    used_ram <- sum(memory.info[,2]) * 1024
    ratio <- round(used_ram / total_ram, 2) * 100
    span_text <- sprintf("%s / %s", convertkb2size(used_ram),
                         convertkb2size(total_ram))
    html_text <- sprintf("%s\n%s", html_text,
                         set_monitor_progress_bar("memory-monitor", ratio, span_text))
    # tasks monitor
    queue_obj <- ensure_queue(shiny_queue_name, db = queue_db)
    tasks_tb <- list_messages(queue_obj)
    tasks.total <- nrow(tasks_tb)
    tasks.queue <- nrow(tasks_tb[tasks_tb$status == "READY",])
    tasks.running <- tasks.total - tasks.queue
    if (tasks.total == 0) {
      ratio <- 0
      span_text <- "No task"
    } else {
      ratio <- round(tasks.running / tasks.total, 2) * 100
      span_text <- sprintf("Running(%s) / Total(%s)", tasks.running, tasks.total)
    }
    html_text <- sprintf("%s\n%s;", html_text,
                         set_monitor_progress_bar("queue-tasks", ratio, span_text))
    if (input$dashbord_auto_update == 0){
      html_text <- sprintf("%s\nfunction dashbord_auto_update()
                           {$('#dashbord_auto_update').click();}; setInterval(dashbord_auto_update,'10000');</script>", html_text)
    } else {html_text <- sprintf("%s</script>", html_text)}
    HTML(html_text)
    })
  return(output)
}

dashbord_section_server <- function(input, output) {

  update_system_monitor(input, output)

  output$r_session_info_monitor <- renderPrint({
    print(sessionInfo())
  })

  output <- custom_render_DT(output, "r_packages_info_monitor", "installed.packages()")
  output <- custom_render_DT(output, "r_env_info_monitor", "x <- Sys.getenv(); y <- unname(x); attributes(y) <- NULL; data.frame(var=names(x), value=y)")

  render_task_table <- function() {
    req(input$task_table_key)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    result <- NULL
    if (input$task_table_key == task_table_admin_key) {
      sql <- sprintf("SELECT * FROM %s", task_table)
      result <- DBI::dbGetQuery(con, sql)
    } else {
      sql <- sprintf("SELECT * FROM %s WHERE key = '%s'", task_table, input$task_table_key)
      result <- DBI::dbGetQuery(con, sql)
    }
    DBI::dbDisconnect(con)
    return(result)
  }
  observeEvent(input$task_table_button, {
    func <- render_task_table()
    output <- custom_render_DT(output, "task_table_DT", func = func)
    output$task_table_log <- renderPrint({
      for(fn in eval(func)$log) {
         cat(sprintf("\n\n-------------------------- %s ------------------------\n", fn))
         cat(paste0(readLines(fn), collapse = '\n'))
      }
    })
  })
  return(output)
}
