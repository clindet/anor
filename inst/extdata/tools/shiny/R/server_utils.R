generate_server_object <- function(input, output, ui_server_config, toolname, pkgs = NULL) {
  if (!is.null(pkgs)) {sapply(pkgs, function(x){require(x, character.only = TRUE)})}
  ui.sections <- ui_server_config[[toolname]]$ui$sections
  params <- ui_server_config[[toolname]]$paramters
  total_box_num <- length(ui.sections$order)
  for(item in ui.sections$order) {
    output <- render_input_command(input, output, params, item)
  }
  start_trigger <- sprintf("start_%s_analysis", toolname)
  observeEvent(input[[start_trigger]], {
    shinyjs::disable(start_trigger)
    progress <- shiny::Progress$new()
    msg <- sprintf("Running %s analysis steps,  waiting please...", toolname)
    for (i in 1:100) {
      progress$set(message = msg, value = i/100)
      Sys.sleep(0.02)
    }
    on.exit({
      progress$close();
      shinyjs::enable(start_trigger)
    })
    item_num <- 1
    for (item in ui.sections$order) {
      check_sprintf <- params[[item]][["rcmd_last_sprintf"]]
      req_eval <- !is.null(check_sprintf) && check_sprintf
      if (params[[item]]$section_type == "input") {
        for (ui_section in params[[item]]$input_ui_order) {
          input_section <- params[[item]]$input[[ui_section]]
          if (!is.null(input_section$varname)) {
            for (var_idx in 1:length(input_section$varname)) {
              assign(input_section$varname[var_idx], input[[input_section$input_id[var_idx]]], envir = globalenv())
            }
          }
        }
        cmd <- clean_parsed_item(input[[paste0("lastcmd_", params[[item]]$render_id)]], FALSE)
        if (is.null(cmd)) {
          cmd <- clean_parsed_item(params[[item]][["rcmd_last"]], req_eval)
        }
        eval(parse(text = cmd), envir = globalenv())
      }
      for (rcmd in c("rcmd_preprocess")) {
        if (rcmd %in% names(params[[item]])) {
          cmd <- input[[paste0(rcmd, "_", params[[item]]$render_id)]]
          if (is.null(cmd)) {
            check_sprintf_2 <- params[[item]][[paste0(rcmd, "_sprintf")]]
            req_eval_2 <- !is.null(check_sprintf_2) && check_sprintf_2
            cmd <- clean_parsed_item(params[[item]][[rcmd]], req_eval_2)
          }
          eval(parse(text = cmd), envir = globalenv())
        }
      }
      render_type <- params[[item]]$render_type
      cmd <- clean_parsed_item(input[[paste0("lastcmd_", params[[item]]$render_id)]], FALSE)
      if (!is.null(render_type) && render_type == "shiny::renderDataTable") {
        render_tool_DT <- function(item) {
          DT_opt <- set_DT_opt(params, item)
          if (is.null(cmd)) {
            cmd <- clean_parsed_item(params[[item]][["rcmd_last"]], req_eval)
          }
          render_dat <- eval(parse(text = cmd))
          cmd <- sprintf("output$%s <- %s({render_dat}, options = DT_opt)",
            params[[item]]$render_id, render_type)
          eval(parse(text = cmd), envir = globalenv())

          output <- update_hander_from_params(input, output, toolname, params, item, update_hander_DT_func,
                                              other_object = list(DT_opt = DT_opt))

        }
        render_tool_DT(item)
      } else if (!is.null(render_type)) {
        render_tool_plot <- function(item) {
          render_params <- params[[item]]$render_params
          if (is.null(render_params))  render_params <- ""
          else render_params <- paste0(', ', render_params)
          if (is.null(cmd)) {
            cmd <- sprintf("%s({%s}%s)", params[[item]]$render_type,
                           clean_parsed_item(params[[item]]$rcmd_last, req_eval), render_params)
          } else {
            cmd <- sprintf("%s({%s}%s)", params[[item]]$render_type, cmd, render_params)
          }
          output[[params[[item]]$render_id]] <- eval(parse(text = cmd), envir = globalenv())
          export_id <- paste0("export_", params[[item]]$render_id)
          output <- download_hander_from_params(input, output, params, item, req_eval)
          output <- update_hander_from_params(input, output, toolname, params, item, update_hander_pdf_func)
        }
        render_tool_plot(item)
      }
      progress$set(message = params[[item]]$progressbar_message, value = 1)
      item_num <- item_num + 1
      shinyjs::enable(paste0("update_", params[[item]]$render_id))
      shinyjs::enable(paste0("export_", params[[item]]$render_id))
    }
  })
  return(output)
}

download_hander_from_params <- function(input, output, params, item, req_eval) {
  export_id <- paste0("export_", params[[item]]$render_id)
  export_dev <- params[[item]]$export_engine
  export_dev_param <- sprintf("list(%s)", params[[item]]$export_params)
  export_dev_param <- eval(parse(text = export_dev_param))
  output[[export_id]] <- downloadHandler(sprintf("%s_%s.%s",
     export_id, stringi::stri_rand_strings(1, 6), export_dev_param$type),
     function(theFile) {
       make_pdf <- function(filename){
         export_dev_param$width <- input[[paste0(export_id, "_width")]]
         export_dev_param$height <- input[[paste0(export_id, "_height")]]
         export_dev_param$file <- filename
         cmd <- clean_parsed_item(input[[paste0("lastcmd_", params[[item]]$render_id)]], FALSE)
         if (is.null(cmd)) {
           cmd <- clean_parsed_item(params[[item]]$rcmd_last, req_eval)
         }
         if (export_dev == "link") {
           obj_list <- eval(parse(text = cmd), envir = globalenv())
           file.copy(obj_list$src, filename)
         }  else {
           base::do.call(eval(parse(text = export_dev)), export_dev_param)
           eval(parse(text = cmd), envir = globalenv())
           dev.off()
         }
       }
         progress <- shiny::Progress$new()
         for (i in 1:100) {
           progress$set(message = "Exporting...",
                              value = i/100)
           Sys.sleep(0.01)
         }
         make_pdf(theFile)
           on.exit(progress$close())
         })
  return(output)
}

update_hander_pdf_func <- function(input, output, params, item, render_type, render_id, other_object = NULL) {
  cmd <- clean_parsed_item(input[[paste0("rcmd_preprocess_", render_id)]], FALSE)
  print(cmd)
  if (!is.null(cmd)) eval(parse(text = cmd), envir = globalenv())
  cmd <- clean_parsed_item(input[[paste0("lastcmd_", render_id)]], FALSE)
  cmd <- sprintf("%s({%s})", render_type, cmd)
  output[[render_id]] <- eval(parse(text = cmd), envir = globalenv())
  return(output)
}

update_hander_DT_func <- function(input, output, params, item, render_type, render_id, other_object = NULL) {
  DT_opt <- other_object$DT_opt
  cmd <- clean_parsed_item(input[[paste0("rcmd_preprocess_", render_id)]], FALSE)
  if (!is.null(cmd)) eval(parse(text = cmd), envir = globalenv())
  cmd <- clean_parsed_item(input[[paste0("lastcmd_", render_id)]], FALSE)
  render_dat <- eval(parse(text = cmd), envir = globalenv())

  cmd <- sprintf("%s({%s}, options = DT_opt)", render_type, cmd)
  output[[render_id]] <- eval(parse(text = cmd))
  return(output)
}

set_DT_opt <- function(params, item) {
  DT_opt <- params[[item]]$render_DT_options
  DT_opt <- eval(parse(text = clean_parsed_item(DT_opt)))
  DT_opt <- list(pageLength = 10, lengthMenu = list(list(5, 10, 25,
    50, -1), list(5, 10, 25, 50, "All")), dom = "Bfrtlip", buttons = c("copy",
             "csv", "excel", "pdf", "print"))
}

update_hander_from_params <- function(input, output, toolname = "maftools", params, item, eval_func, other_object = NULL) {
  render_id <- params[[item]]$render_id
  update_id <- paste0("update_", render_id)
  render_type <- params[[item]]$render_type
  start_trigger <- sprintf("start_%s_analysis", toolname)
  if (input[[start_trigger]] == 1) {
    observeEvent(input[[update_id]], {
      shinyjs::disable(update_id)
      progress <- shiny::Progress$new()
      for (i in 1:100) {
        progress$set(message = "Updating...",
                     value = i/100)
        Sys.sleep(0.01)
      }
      on.exit({
        progress$close()
        shinyjs::enable(update_id)
      })

      output <- do.call(eval_func, list(input = input,
                        output = output, params = params, item = item,
                        render_type = render_type,
                        render_id = render_id, other_object = other_object))
    })
  }
  return(output)
}

render_input_command <- function (input, output, params, item){
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
  if (length(cmd_2) != 0)
    output[[paste0("rcmd_preprocess_ui_", render_id)]] <- renderUI({
      shiny::textAreaInput(inputId = paste0("rcmd_preprocess_",render_id), label = "R commands for preprocess",
                           value = cmd_2, rows = stringr::str_count(cmd_2, "\n") * 1.5, resize = "vertical")
    })
  return(output)
}

maftools_server <- function(input, output, pkgs = "maftools"){
  output <- generate_server_object(input, output, config.maftools, "maftools", pkgs)
}

gvmap_server <- function(input, output, pkgs = "gvmap"){
  output <- generate_server_object(input, output, config.gvmap, "gvmap", pkgs)
}

