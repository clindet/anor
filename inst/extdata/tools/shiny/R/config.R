skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)

# Read configuration file and set the environment vars
config.file <-  Sys.getenv("ANNOVARR_SHINY_CONFIG",
                           system.file("extdata", "config/shiny.config.toml", package = "annovarR"))
config <- read.config(config.file, file.type = "toml")
db_type <- config$shiny_db$db_type
db_path <- normalizePath(config$shiny_db$db_path, mustWork = FALSE)
upload_table <- config$shiny_db_table$upload_data_table_name
upload_table_colnames <- config$shiny_db_table$upload_data_table_colnames
upload_dir <- normalizePath(config$shiny_upload$upload_dir, mustWork = FALSE)


options(shiny.maxRequestSize=30000*1024^2)


if (skin == "")
  skin <- "blue"
