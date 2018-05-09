maftools_server <- function(input, output) {
  observeEvent(input$start_maftools_analysis, {
    progress <- shiny::Progress$new()
    for (i in 1:100) progress$set(message = "Running maftools analysis steps,  waiting please...",
      value = i/100)
    Sys.sleep(0.03)
    on.exit(progress$close())
    item_num <- 1
    ui.sections <- config.maftools$maftools$ui$sections
    params <- config.maftools$maftools$paramters
    total_box_num <- length(ui.sections$order)
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
          if ("ui_pre" %in% names(input_section)) {
          for (code_block in names(input_section$ui_pre)) {
            render_ui_pre <- function(code_block) {
            render_id_pre <- paste0(item, "_", code_block, "_pre")
            out_content <- eval(parse(text = sprintf("params$%s", input_section$ui_pre[[code_block]])))
            output[[render_id_pre]] <- renderPrint({
              out_content <- clean_parsed_item(out_content)
              cat(out_content)
            })
            }
            render_ui_pre(code_block)
          }
          }
        }
        cmd <- clean_parsed_item(params[[item]][["rcmd_last"]], req_eval)
        eval(parse(text = cmd))
      }
      for (rcmd in c("rcmd_preprocess")) {
        if (rcmd %in% names(params[[item]])) {
          check_sprintf_2 <- params[[item]][[paste0(rcmd, "_sprintf")]]
          req_eval_2 <- !is.null(check_sprintf_2) && check_sprintf_2
          eval(parse(text = clean_parsed_item(params[[item]][[rcmd]]), req_eval_2))
        }
      }
      render_type <- params[[item]]$render_type
      if (!is.null(render_type) && render_type == "shiny::renderDataTable") {

        DT_opt <- params[[item]]$render_DT_options
        DT_opt <- eval(parse(text = clean_parsed_item(DT_opt)))
        DT_opt <- list(pageLength = 10, lengthMenu = list(list(5, 10, 25,
          50, -1), list(5, 10, 25, 50, "All")), dom = "Bfrtlip", buttons = c("copy",
          "csv", "excel", "pdf", "print"))
        render_dat <- eval(parse(text = clean_parsed_item(params[[item]]$rcmd_last,
          req_eval)))
        cmd <- sprintf("output$%s <- %s({render_dat}, options = DT_opt)",
          params[[item]]$render_id, render_type)
        eval(parse(text = cmd))
      } else if (!is.null(render_type)) {
        render_fun <- function(item) {
          cmd <- sprintf("%s({%s})", params[[item]]$render_type, clean_parsed_item(params[[item]]$rcmd_last,
          req_eval))
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

                 cmd <- clean_parsed_item(params[[item]]$rcmd_last,
                                          req_eval)
                 base::do.call(eval(parse(text = export_dev)), export_dev_param)
                 eval(parse(text = cmd))
                 dev.off()
               }
               make_pdf(theFile)
             })

        }
        render_fun(item)
      }
      progress$set(message = params[[item]]$progressbar_message, value = 1)
      item_num <- item_num + 1
    }
  })
  return(output)
}

