source("config.R")
clean_parsed_item <- function(item, req_parse = FALSE) {
  if (is.null(item)) {
    return(NULL)
  }
  item <- stringr::str_replace_all(item, "\\\\n", "\n")
  item <- stringr::str_replace_all(item, "\\\\\"", "\"")
  item <- stringr::str_replace_all(item, "‘", "'")
  item <- stringr::str_replace_all(item, "’", "'")
  item <- stringr::str_replace_all(item, "，", ", ")
  item <- stringr::str_replace_all(item, '“', '"')
  item <- stringr::str_replace_all(item, '”', '"')
  if (req_parse) {
    return(sprintf("eval(parse(text = %s))", item))
  }
  item
}
generate_boxes_object <- function(config, tool_name) {
  items <- config[[tool_name]]$ui$sections$order
  sapply(items, function(item) {
    basic_params <- config[[tool_name]]$ui$sections$ui_basic[[item]]
    section_type <- config[[tool_name]]$paramters[[item]]$section_type
    if (section_type == "input") {
      input_sections <- config[[tool_name]]$paramters[[item]]$input_ui_order
      advanced_params <- ""
      for (input_section in input_sections) {
        input_section_dat <- config[[tool_name]]$paramters[[item]]$input[[input_section]]
        section_type <- input_section_dat$type
        title <- input_section_dat$title
        title_control <- input_section_dat$title_control
        label <- input_section_dat$label
        choices <- input_section_dat$choices
        selected <- input_section_dat$selected
        varname <- input_section_dat$varname
        input_id <- input_section_dat$input_id
        if (!is.null(title) && !is.null(title_control)) {
          advanced_params <- sprintf("%s, shiny::p('%s', %s)", advanced_params,
          title, title_control)
        } else if (!is.null(title)) {
          advanced_params <- sprintf("%s, shiny::p('%s')", advanced_params)
        }
        for (id_index in 1:length(input_id)) {

          var <- varname[id_index]
          advanced_params <- sprintf("%s, %s(inputId='%s', label = '%s'",
          advanced_params, section_type[id_index], input_id[id_index],
          label[id_index])
          for (param_name in c("choices", "selected", "width", "multiple",
          "buttonLabel", "placeholder", "value", "height", "rows", "cols",
          "resize")) {
            if (param_name %in% names(input_section_dat)) {
              if (is.null(names(input_section_dat[[param_name]]))) var <- id_index
              param_value <- input_section_dat[[param_name]][[var]]
              if (is.null(param_value)) next
              if (is.character(param_value) && length(param_value) == 1) {
                advanced_params <- sprintf("%s, %s='%s'", advanced_params,
                  param_name, input_section_dat[[param_name]][[var]])
              } else if (length(param_value) == 1) {
                advanced_params <- sprintf("%s, %s=%s", advanced_params,
                  param_name, input_section_dat[[param_name]][[var]])
              } else if (is.character(param_value)) {
                val_paste <- paste0(input_section_dat[[param_name]][[var]], collapse = "','")
                advanced_params <- sprintf("%s, %s=c('%s')", advanced_params,
                  param_name, val_paste)
              } else {
                val_paste <- paste0(input_section_dat[[param_name]][[var]], collapse = ',')
                advanced_params <- sprintf("%s, %s=c(%s)", advanced_params,
                                           param_name, val_paste)
              }
            }
          }
          advanced_params <- sprintf("%s)", advanced_params)
        }
      }
      advanced_params <- sprintf("%s, hr(), shiny::uiOutput('lastcmd_ui_%s')",
                                 advanced_params, config[[tool_name]]$paramters[[item]]$render_id)
    } else {
      render_id <- config[[tool_name]]$paramters[[item]]$render_id
      render_type <- config[[tool_name]]$paramters[[item]]$render_type
      output_type <- config[[tool_name]]$paramters[[item]]$output_type

      advanced_params <- sprintf(", withSpinner(%s(outputId = '%s'), type = 8)",
        output_type, render_id)
      if (render_type %in% c("shiny::renderImage", "shiny::renderPlot")) {

        export_params <- config[[tool_name]]$paramters[[item]]$export_params
        width <- stringr::str_replace(stringr::str_extract(export_params,
          "width = [0-9]*"), "width = ", "")
        height <- stringr::str_replace(stringr::str_extract(export_params,
          "height = [0-9]*"), "height = ", "")
        advanced_params <- sprintf("%s, shiny::uiOutput('rcmd_preprocess_ui_%s')",
                                   advanced_params, render_id)
        advanced_params <- sprintf("%s, shiny::uiOutput('lastcmd_ui_%s')",
                                   advanced_params, render_id)
        if (tool_name != "gvmap") {
        advanced_params <- sprintf("%s, shiny::column(6, wellPanel (shiny::sliderInput('export_%s_height', min = 0, max = 100, value = %s, label = 'height (cm)')))",
          advanced_params, render_id, height)
        advanced_params <- sprintf("%s, shiny::column(6, wellPanel (shiny::sliderInput('export_%s_width', min = 0, max = 100, value = %s, label = 'width (cm)')))",
          advanced_params, render_id, width)
        }
        advanced_params <- sprintf("%s, shiny::downloadButton('export_%s', label = 'Export', disabled = 'disabled')",
          advanced_params, render_id)
        advanced_params <- sprintf("%s, shiny::actionButton('update_%s', label = 'Update plot', icon = icon('refresh'), disabled = 'disabled')",
                                   advanced_params, render_id)
      } else {
        advanced_params <- sprintf("%s, shiny::uiOutput('rcmd_preprocess_ui_%s')",
                                   advanced_params, render_id)
        advanced_params <- sprintf("%s, shiny::uiOutput('lastcmd_ui_%s')",
                                   advanced_params, render_id)
        advanced_params <- sprintf("%s, shiny::actionButton('update_%s', label = 'Update output', icon = icon('refresh'), disabled = 'disabled')",
                                   advanced_params, render_id)
      }
    }

    cmd <- sprintf("box(%s%s)", basic_params, advanced_params)
    return(cmd)
  })
}
# Generate maftools UI
config.maftools <- configr::read.config(system.file("extdata", "config/shiny.maftools.parameters.toml",
  package = "annovarR"), rcmd.parse = TRUE, glue.parse = TRUE, file.type = "toml")
maftools_boxes <- generate_boxes_object(config.maftools, "maftools")

config.gvmap <- configr::read.config(system.file("extdata", "config/shiny.gvmap.parameters.toml",
                   package = "annovarR"), rcmd.parse = TRUE, glue.parse = TRUE, file.type = "toml")
gvmap_boxes <- generate_boxes_object(config.gvmap, "gvmap")

config.clusterProfiler <- configr::read.config(system.file("extdata",
                                                 "config/shiny.clusterProfiler.parameters.toml",
                                                 package = "annovarR"), rcmd.parse = TRUE, glue.parse = TRUE, file.type = "toml")
clusterProfiler_boxes <- generate_boxes_object(config.clusterProfiler, "clusterProfiler")

cmd <- sprintf(paste0("body_visulization_tabItem <- tabItem('visulization',",
                   "tabsetPanel(type = 'pills',",
                      "tabPanel('maftools',",
                        "fluidRow( %s )",
                      "),",
                      "tabPanel('gvmap',",
                        "fluidRow( %s )",
                      "),",
                      "tabPanel('clusterProfiler',",
                        "fluidRow( %s )",
                      ")",
                   ")",
                  ")"), paste0(unname(maftools_boxes), collapse = ","),
               paste0(unname(gvmap_boxes), collapse = ","),
               paste0(unname(clusterProfiler_boxes), collapse = ","))

body_visulization_tabItem <- eval(parse(text = cmd))
