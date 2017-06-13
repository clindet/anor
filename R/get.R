#' Show top n line of table of database in sqlite database
#'
#' @param sqlite.connect.params Connect to sqlite database params [sqlite.path, table.name]
#' @param n n lines will be selected
#' @param extra.sql Extra sql statement
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters pass to dbGetQuery
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, list(sqlite.path = test.sqlite, 
#' table.name = 'snp_test'))
#' sqlite.head(list(sqlite.path = test.sqlite, table.name = 'snp_test'))
sqlite.head <- function(sqlite.connect.params = list(sqlite.path = "", table.name = ""), 
  n = 10, extra.sql = NULL, verbose = FALSE, ...) {
  if (names(sqlite.connect.params)[1] != "") {
    sqlite.connect.params <- config.list.merge(list(sqlite.connect.params[["sqlite.path"]]), 
      sqlite.connect.params)
  }
  sqlite.connect.params <- config.list.merge(list(SQLite()), sqlite.connect.params)
  sqlite.db <- do.call(dbConnect, sqlite.connect.params)
  sql <- sprintf("SELECT * FROM '%s' LIMIT %s", sqlite.connect.params[["table.name"]], 
    n)
  sql <- paste0(sql, extra.sql)
  info.msg(sprintf("Query sql: %s", sql), verbose = verbose)
  nlines <- dbGetQuery(sqlite.db, sql, ...)
  dbDisconnect(sqlite.db)
  return(nlines)
}
#' Show top n line of table of database in mysql database
#'
#' 
#' @param mysql.connect.params Mysql parameters, [host, dbname, table.name, user, password etc.]
#' @param n n lines will be selected
#' @param extra.sql Extra sql statement
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters pass to dbGetQuery
#' @export
#' @examples
#' host <- '11.11.11.1'
mysql.head <- function(mysql.connect.params = list(host = "", dbname = "", table.name = "", 
  user = "", password = ""), n = 10, extra.sql = NULL, verbose = FALSE, ...) {
  mysql.connect.params <- config.list.merge(list(MySQL()), mysql.connect.params)
  mysql.db <- do.call(dbConnect, mysql.connect.params)
  sql <- sprintf("SELECT * FROM %s LIMIT %s", mysql.connect.params[["table.name"]], 
    n)
  sql <- paste0(sql, extra.sql)
  info.msg(sprintf("Query sql: %s", sql), verbose = verbose)
  nlines <- dbGetQuery(mysql.db, sql, ...)
  dbDisconnect(mysql.db)
  return(nlines)
}

#' Get all annovarR supported databases 
#'
#' @param database.cfg Configuration file of annovarR databases infomation
#' @export
#' @examples
#' cfg <- system.file('extdata', 'config/config.toml', package = 'annovarR')
#' get.annotation.names(cfg) 
get.annotation.names <- function(database.cfg = system.file("extdata", "config/databases.toml", 
  package = "annovarR")) {
  config <- configr::read.config(file = database.cfg)
  config <- config[names(config) != "Title"]
  return(unname(unlist(lapply(config, function(x) x["versions"]))))
}

#' Get annovarR default databases type [sqlite, txt]
#'
#' @param name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param database.cfg Configuration file of annovarR databases infomation
#' @export
#' @examples
#' get.annotation.dbtype('avsnp147') 
get.annotation.dbtype <- function(name, database.cfg = system.file("extdata", "config/databases.toml", 
  package = "annovarR")) {
  dbtype <- get.cfg.value.by.name(name, database.cfg, key = "default.dbtype")
  return(dbtype)
}

#' Get colnames of table of database in sqlite
#'
#' @param sqlite.connect.params Connect to sqlite database params [sqlite.path, table.name]
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, list(sqlite.path = test.sqlite, 
#' table.name = 'snp_test'))
#' sqlite.tb.colnames(list(sqlite.path = test.sqlite, table.name = 'snp_test'))
sqlite.tb.colnames <- function(sqlite.connect.params = list(sqlite.path = "", table.name = "")) {
  if (names(sqlite.connect.params)[1] != "") {
    sqlite.connect.params <- config.list.merge(list(sqlite.connect.params[["sqlite.path"]]), 
      sqlite.connect.params)
  }
  sqlite.connect.params <- config.list.merge(list(SQLite()), sqlite.connect.params)
  sqlite.db <- do.call(dbConnect, sqlite.connect.params)
  sql <- sprintf("PRAGMA table_info([%s])", sqlite.connect.params[["table.name"]])
  table.info <- dbGetQuery(sqlite.db, sql)
  tb.colnames <- table.info[, "name"]
  dbDisconnect(sqlite.db)
  return(tb.colnames)
}

#' Get colnames of table of database in mysql
#'
#' @param mysql.connect.params Mysql parameters, [host, dbname, table.name, user, password etc.]
#' @param ... Other parameters pass to dbConnect
#' @export
#' @examples
#' ##mysql.db.colnames(list(host = 'host', dbname = 'db', user = 'user', 
#' ##password = 'password', table.name = 'table'))
mysql.tb.colnames <- function(mysql.connect.params = list(host = "", dbname = "", 
  user = "", password = "", table.name = ""), ...) {
  mysql.connect.params <- config.list.merge(list(MySQL()), mysql.connect.params)
  mysql.db <- do.call(dbConnect, mysql.connect.params)
  sql <- sprintf("DESC %s", mysql.connect.params[["table.name"]])
  table.info <- dbGetQuery(mysql.db, sql)
  tb.colnames <- table.info$Field
  dbDisconnect(mysql.db)
  return(tb.colnames)
}

#' Get sqlite table index
#'
#' @param sqlite.connect.params Connect to sqlite database params [sqlite.path, table.name]
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' params <- list(sqlite.path = test.sqlite,
#' table.name = 'snp_test')
#' x <- sqlite.build(filename = test.dat, params)
#' x <- sqlite.index(params, index = 'index4', cols = c('V1', 'V2'))
#' indexes <- sqlite.tb.indexes(params)
#' test.sqlite <- normalizePath(test.sqlite, '/')
#' file.remove(test.sqlite)
sqlite.tb.indexes <- function(sqlite.connect.params = list(sqlite.path = "", table.name = "")){
  sqlite.db <- sqlite.connect.initial(sqlite.connect.params, verbose = FALSE)
  sql <- "SELECT * FROM sqlite_master WHERE type = 'index'"
  indexes <- dbGetQuery(sqlite.db, sql)
  dbDisconnect(sqlite.db)
  return(indexes)
}
