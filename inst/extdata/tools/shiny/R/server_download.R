source("config.R")
parse_buildver_object <- function(download.name, download.version) {
    buildver_object <- annovarR::download.database(download.name,
                      version = download.version,
                      show.all.buildvers = TRUE)
    x <- unique(unname(unlist(buildver_object)))
    return(x)
}

download_section_server <- function(input, output) {
  output$database.dir.input <- renderUI(
    textInput("database.dir", "Directory stored databases",
              download_dir)
    )

  observeEvent(input$download_run, {
    req(input$download.name)
    req(input$download.version)

      if (input$database.dir == "") {
        database.dir <- tempdir()
      } else {
        database.dir <- input$database.dir
      }
      version <- input$download.version
      buildver <- input$download.buildver
      if (any(input$download.version == "")) {
        index <- input$download.version == ""
        version[index] <- annovarR::download.database(download.name = input$download.name[index],
                                                                   show.all.versions = TRUE)[1]
        input$download.version <- version
      }
      if (any(input$download.buildver == "")) {
        index <- input$download.buildver == ""
        buildver[index] <- annovarR::download.database(download.name = input$download.name[index],
                                             version = version[index], show.all.buildvers = TRUE)[1]
      }
      if (any(input$download.name != "")) {
        extra.params <- eval(parse(text = input$extra.paramters))
        params <- list(download.name = input$download.name, database.dir = database.dir,
                       verbose = TRUE, version = version, buildver = buildver)
        params <- config.list.merge(params, extra.params)
        progress <- shiny::Progress$new()
        for (i in 1:100) progress$set(message = "Submitting task...",
                                      value = i/100)
        Sys.sleep(2)
        on.exit(progress$close())
        params$qqcommand <- "annovarR::download.database"
        params$qqkey <- stringi::stri_rand_strings(1, 50)
        params$qqcommand_type <- "R"
        msg <- jsonlite::toJSON(params)
        queue <- liteq::ensure_queue(shiny_queue_name, db = queue_db)
        while(TRUE) {
          tryCatch({liteq::publish(queue, title = "Download", message = msg);break},
                   error = function(e) {})
        }
        output <- dashbord_section_server(input, output)
        output$task_submit_modal <- renderUI({
          html_text <- tryCatch(get("html_text_task_submit_modal", envir = globalenv()), error = function(e) {
            html_text <- paste0(readLines("www/modal.html"), collapse = "\n")
            assign("html_text_task_submit_modal", html_text, envir = globalenv())
            return(html_text)
          })
          html_text <- str_replace_all(html_text, '\\{\\{task_title\\}\\}', "Downloader")
          html_text <- str_replace_all(html_text, '\\{\\{task_key\\}\\}', params$qqkey)
          html_text <- str_replace_all(html_text, '\\{\\{task_msg\\}\\}', msg)
          html_text <- sprintf("%s<script>$('#myModal').modal('show')</script>", html_text)
          HTML(html_text)
        })
      }
  })
  observeEvent(input$download.name, {
    output$download.version.selector <- shiny::renderUI({
      selectInput("download.version", "Version of Database",
                  choices = annovarR::download.database(input$download.name, show.all.versions = TRUE), multiple = TRUE)
    })
    observeEvent(input$download.version, {
      output$download.buildver.selector <- shiny::renderUI({
        selectInput("download.buildver", "Buildver version of Database",
                    choices = parse_buildver_object(download.name = input$download.name,
                                                    download.version = input$download.version),
                    multiple = FALSE)
      })
    })
  })
  return(output)
}
