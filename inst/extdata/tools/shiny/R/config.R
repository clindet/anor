source("global_var.R")
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

update_configuration_files()


