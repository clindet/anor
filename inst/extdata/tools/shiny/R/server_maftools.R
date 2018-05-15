
maftools_server <- function(input, output) {
  ui.sections <- config.maftools$maftools$ui$sections
  params <- config.maftools$maftools$paramters
  total_box_num <- length(ui.sections$order)
  for(item in ui.sections$order) {
    render_maftools_cmd <- function(item) {
      render_id <- params[[item]]$render_id
      check_sprintf <- params[[item]][["rcmd_last_sprintf"]]
      req_eval <- !is.null(check_sprintf) && check_sprintf
      cmd <- clean_parsed_item(params[[item]][["rcmd_last"]], req_eval)
      label = "R commands for plot"
      if (params[[item]]$section_type == "input") label <- "R commands for reading files"
      output[[paste0("lastcmd_ui_", render_id)]] <- renderUI({
        shiny::textAreaInput(inputId = paste0("lastcmd_", render_id), label = label,
                             value = cmd, rows = stringr::str_count(cmd, "\n") * 1.5, resize = "vertical")
      })
      check_sprintf_2 <- params[[item]][["rcmd_preprocess_sprintf"]]
      req_eval_2 <- !is.null(check_sprintf_2) && check_sprintf_2
      cmd_2 <- clean_parsed_item(params[[item]][["rcmd_preprocess"]], req_eval)
      if (length(cmd_2) != 0){
        output[[paste0("rcmd_preprocess_ui_", render_id)]] <- renderUI({
          shiny::textAreaInput(inputId = paste0("rcmd_preprocess_",render_id), label = "R commands for preprocess",
                               value = cmd_2, rows = stringr::str_count(cmd_2, "\n") * 1.5, resize = "vertical")
        })
      }
    }
    render_maftools_cmd(item)
  }
  observeEvent(input$start_maftools_analysis, {
    progress <- shiny::Progress$new()
    for (i in 1:100) progress$set(message = "Running maftools analysis steps,  waiting please...",
      value = i/100)
    Sys.sleep(1)
    on.exit(progress$close())
    item_num <- 1

    # shinyjs::runjs('$('div.pull-right:gt(1) button').click();')
    for (item in ui.sections$order) {
      # Set progress bar for every box
      check_sprintf <- params[[item]][["rcmd_last_sprintf"]]
      req_eval <- !is.null(check_sprintf) && check_sprintf
      if (params[[item]]$section_type == "input") {
        for (ui_section in params[[item]]$input_ui_order) {
          input_section <- params[[item]]$input[[ui_section]]
          if (!is.null(input_section$varname)) {
            for (var_idx in 1:length(input_section$varname)) {
              assign(input_section$varname[var_idx], input[[input_section$input_id[var_idx]]])
            }
          }
        }
        cmd <- clean_parsed_item(input[[paste0("lastcmd_", params[[item]]$render_id)]], FALSE)
        if (is.null(cmd)) {
          cmd <- clean_parsed_item(params[[item]][["rcmd_last"]], req_eval)
        }
        suppressMessages(eval(parse(text = cmd)))
      }
      for (rcmd in c("rcmd_preprocess")) {
        if (rcmd %in% names(params[[item]])) {
          cmd <- input[[paste0(rcmd, "_", params[[item]]$render_id)]]
          if (is.null(cmd)) {
            check_sprintf_2 <- params[[item]][[paste0(rcmd, "_sprintf")]]
            req_eval_2 <- !is.null(check_sprintf_2) && check_sprintf_2
            cmd <- clean_parsed_item(params[[item]][[rcmd]], req_eval_2)
          }
          eval(parse(text = cmd))
        }
      }
      render_type <- params[[item]]$render_type
      cmd <- clean_parsed_item(input[[paste0("lastcmd_", params[[item]]$render_id)]], FALSE)
      if (!is.null(render_type) && render_type == "shiny::renderDataTable") {

        DT_opt <- params[[item]]$render_DT_options
        DT_opt <- eval(parse(text = clean_parsed_item(DT_opt)))
        DT_opt <- list(pageLength = 10, lengthMenu = list(list(5, 10, 25,
          50, -1), list(5, 10, 25, 50, "All")), dom = "Bfrtlip", buttons = c("copy",
          "csv", "excel", "pdf", "print"))
        if (is.null(cmd)) {
          cmd <- clean_parsed_item(params[[item]][["rcmd_last"]], req_eval)
        }
        render_dat <- eval(parse(text = cmd))
        cmd <- sprintf("output$%s <- %s({render_dat}, options = DT_opt)",
          params[[item]]$render_id, render_type)
        eval(parse(text = cmd))
      } else if (!is.null(render_type)) {
        render_maftools_fun <- function(item) {
          if (is.null(cmd)) {
            cmd <- sprintf("%s({%s})", params[[item]]$render_type,
                           clean_parsed_item(params[[item]]$rcmd_last, req_eval))
          } else {
            cmd <- sprintf("%s({%s})", params[[item]]$render_type, cmd)
          }
          output[[params[[item]]$render_id]] <- eval(parse(text = cmd))
          export_id <- paste0("export_", params[[item]]$render_id)
          output[[export_id]] <- downloadHandler(sprintf("%s_%s.pdf",
             export_id, stringi::stri_rand_strings(1, 6)),
             function(theFile) {
               make_pdf <- function(filename){
                 export_dev <- params[[item]]$export_engine
                 export_dev_param <- sprintf("list(%s)", params[[item]]$export_params)
                 export_dev_param <- eval(parse(text = export_dev_param))
                 export_dev_param$width <- input[[paste0(export_id, "_width")]]
                 export_dev_param$height <- input[[paste0(export_id, "_height")]]
                 export_dev_param$file <- filename
                 cmd <- clean_parsed_item(input[[paste0("lastcmd_", params[[item]]$render_id)]], FALSE)
                 if (is.null(cmd))
                   cmd <- clean_parsed_item(params[[item]]$rcmd_last,
                                            req_eval)
                 base::do.call(eval(parse(text = export_dev)), export_dev_param)
                 eval(parse(text = cmd))
                 dev.off()
               }
               make_pdf(theFile)
             })

          update_id <- paste0("update_", params[[item]]$render_id)
          observeEvent(input[[update_id]], {
            cmd <- clean_parsed_item(input[[paste0("rcmd_preprocess_", params[[item]]$render_id)]], FALSE)
            if (!is.null(cmd)) eval(parse(text = cmd))
            cmd <- clean_parsed_item(input[[paste0("lastcmd_", params[[item]]$render_id)]], FALSE)
            print(cmd)
            cmd <- sprintf("%s({%s})", params[[item]]$render_type, cmd)
            output[[params[[item]]$render_id]] <- eval(parse(text = cmd))
          })
        }
        render_maftools_fun(item)
      }
      progress$set(message = params[[item]]$progressbar_message, value = 1)
      item_num <- item_num + 1
      shinyjs::enable(paste0("update_", params[[item]]$render_id))
      shinyjs::enable(paste0("export_", params[[item]]$render_id))
    }
  })
  return(output)
}

