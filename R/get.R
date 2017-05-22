#' Show top n line of table of database in sqlite
#'
#' @param db.path Path of sqlite database
#' @param table.name Table name in sqlite 
#' @param n n lines will be selected
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, db = test.sqlite, table.name = 'snp_test')
#' sqlite.head(test.sqlite, 'snp_test')
sqlite.head <- function(db.path, table.name, n = 10) {
  sqlite.db <- dbConnect(RSQLite::SQLite(), db.path)
  sql <- sprintf("SELECT * FROM '%s' LIMIT %s", table.name, n)
  nlines <- dbGetQuery(sqlite.db, sql)
  dbDisconnect(sqlite.db)
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
#' @param db.path Path of sqlite database
#' @param table.name Table name in sqlite 
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, db = test.sqlite, table.name = 'snp_test')
#' sqlite.tb.colnames(test.sqlite, 'snp_test')
sqlite.tb.colnames <- function(db.path, table.name) {
  sqlite.db <- dbConnect(RSQLite::SQLite(), db.path)
  sql <- sprintf("PRAGMA table_info([%s])", table.name)
  table.info <- dbGetQuery(sqlite.db, sql)
  tb.colnames <- table.info[, "name"]
  dbDisconnect(sqlite.db)
  return(tb.colnames)
}
