skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)

# Read configuration file and set the environment vars
config.file <- Sys.getenv("ANNOVARR_SHINY_CONFIG", system.file("extdata", "config/shiny.config.toml",
  package = "annovarR"))
config <- read.config(config.file, file.type = "toml")
db_type <- config$shiny_db$db_type
db_path <- normalizePath(config$shiny_db$db_path, mustWork = FALSE)
if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)
upload_table <- config$shiny_db_table$upload_data_table_name
upload_table_colnames <- config$shiny_db_table$upload_data_table_colnames
upload_dir <- normalizePath(config$shiny_upload$upload_dir, mustWork = FALSE)
if (!dir.exists(upload_dir)) dir.create(upload_dir, recursive = TRUE)
download_dir <- normalizePath(config$shiny_download$download_dir, mustWork = FALSE)
if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
task_table <- config$shiny_db_table$task_table_name
task_table_admin_key <- config$shiny_db_table$task_table_admin_key
queue_db <- normalizePath(config$shiny_queue$queue_db, mustWork = FALSE)
if (!dir.exists(dirname(queue_db))) dir.create(dirname(queue_db), recursive = TRUE)
shiny_queue_name = config$shiny_queue$name
log_dir = config$shiny_queue$log_dir
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

options(shiny.maxRequestSize = 30000 * 1024^2)

# initial upload_table in sqlite database

if (db_type == "sqlite") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  if (!upload_table %in% DBI::dbListTables(con)) {
    sql <- system.file("extdata", "sql/upload_table.sql", package = "annovarR")
    annovarR::sql2sqlite(sql, dbname = db_path)
  }
  if (!task_table %in% DBI::dbListTables(con)) {
    sql <- system.file("extdata", "sql/task_table.sql", package = "annovarR")
    annovarR::sql2sqlite(sql, dbname = db_path)
  }
  DBI::dbDisconnect(con)
}


if (skin == "") skin <- "blue"

featch_files <- function(file_types = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  sql <- sprintf("SELECT * FROM %s", upload_table)
  if (!upload_table %in% DBI::dbListTables(con)) {
    info <- matrix(data = NA, nrow = 1, ncol = length(upload_table_colnames))
    info <- as.data.frame(info)
    colnames(info) <- upload_table_colnames
    info <- info[-1, ]
  } else {
    info <- DBI::dbGetQuery(con, sql)
  }
  if (!is.null(file_types))
    info <- info[info$file_type %in% file_types,]
  DBI::dbDisconnect(con)
  return(info)
}

update_configuration_files <- function(){
  for(toolname in c("maftools", "gvmap", "clusterProfiler")) {
    config <-  configr::read.config(system.file("extdata", sprintf("config/shiny.%s.parameters.toml", toolname),
                                                package = "annovarR"), rcmd.parse = TRUE, glue.parse = TRUE, file.type = "toml")
    assign(sprintf("config.%s", toolname), config, envir = globalenv())
  }
}

render_input_box_ui <- function(input, output) {
  for(tool_name in c("maftools", "gvmap", "clusterProfiler")) {
    config <- get(sprintf("config.%s", tool_name), envir = globalenv())
    items <- config[[tool_name]]$ui$sections$order
    for(item in items) {
        if ("input_ui_order" %in% names(config[[tool_name]]$paramters[[item]])) {
          input_sections <- config[[tool_name]]$paramters[[item]]$input_ui_order
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
            for (id_index in 1:length(input_id)) {
              render_ui <- function(id_index) {
                var <- varname[id_index]
                advanced_params <- sprintf("%s(inputId='%s', label = '%s'",
                                           section_type[id_index], input_id[id_index],
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
                cmd <- sprintf("output$%s_ui_output <- renderUI({eval(parse(text = advanced_params))})",
                                 input_id[id_index])
                eval(parse(text = cmd))
              }
              render_ui(id_index)
            }
          }
        }
    }
  }
  return(output)
}


