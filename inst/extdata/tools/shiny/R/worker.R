shiny_config_file =
  Sys.getenv("ANNOVARR_SHINY_CONFIG", system.file("extdata", "config/shiny.config.toml",
                                                  package = "annovarR"))

config <- configr::read.config(shiny_config_file, file.type = "toml")
db_type <- config$shiny_db$db_type
db_path <- normalizePath(config$shiny_db$db_path, mustWork = FALSE)
queue_db <- normalizePath(config$shiny_queue$queue_db, mustWork = FALSE)
task_table <- config$shiny_db_table$task_table_name
shiny_queue_name <- config$shiny_queue$name
log_dir <- config$shiny_queue$log_dir

while (TRUE) {
  tryCatch(queue <- liteq::ensure_queue(shiny_queue_name, queue_db),
           error = function(e) {})
  if (is.list(queue)) {
    break
  }
}

while (TRUE) {
  task <- liteq::try_consume(queue)
  if (!is.null(task)) {
    params <- jsonlite::fromJSON(task$message)
    for (item in c("qqcommand", "qqkey", "qqcommand_type")) {
      assign(item, params[[item]])
      params[[item]] <- NULL
    }
    log_file <- sprintf("%s/%s.log", log_dir, qqkey)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    need.initial <- nrow(DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE key = \"%s\"",
                                                      task_table, qqkey))) == 0
    if (need.initial) {
      id <- RSQLite::dbGetQuery(con, sprintf("SELECT seq from sqlite_sequence where name = \"%s\"", task_table))
      if (nrow(id) == 0) {
        id <- 1
      } else {
        id <- as.numeric(id) + 1
      }

      dt <- data.frame(id = id, msgid = task$id, key = qqkey, status = "READY",
        log = log_file)
      DBI::dbWriteTable(con, task_table, dt, append = TRUE)
    }
    DBI::dbSendQuery(con, sprintf("UPDATE %s SET status = \"RUNNING\" WHERE key = \"%s\";",
                                  task_table, qqkey))

    if (is.null(qqcommand_type))
      qqcommand_type <- "r"
    status <- FALSE
    if (tolower(qqcommand_type) == "r") {
      cat(sprintf("%s Running R command for key %s
", format(Sys.time(), "%Y %m-%d %X"), qqkey))
      log_con <- file(log_file)
      sink(log_con, append = TRUE)
      sink(log_con, append = TRUE, type = "message")
      status <- tryCatch(do.call(eval(parse(text = qqcommand)), params),
                         error = function(e) {
                           FALSE
                         })
      sink()
      sink(type = "message")
    }
    if (is.logical(status) && !status) {
      DBI::dbSendQuery(con, sprintf("UPDATE %s SET status = \"FAILD\" WHERE key = \"%s\";",
                                    task_table, qqkey))
    } else {
      DBI::dbSendQuery(con, sprintf("UPDATE %s SET status = \"FINISHED\" WHERE key = \"%s\";",
                                    task_table, qqkey))
    }
    DBI::dbDisconnect(con)
    liteq::ack(task)
    liteq::remove_failed_messages(queue)
  } else {
    Sys.sleep(10)
  }
}
