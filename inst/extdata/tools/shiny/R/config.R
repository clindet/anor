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
