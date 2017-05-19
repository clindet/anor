#' Build annovarR database in sqlite
#'
#' @param filename Path of raw data
#' @param db Output path of sqlite database
#' @param table.name Table name in sqlite 
#' @param new.colnames New colnames of table, default is to retain the original
#' @param overwrite Ligical indicating wheather overwrite sqlite database
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters be used in \code{\link[data.table]{fread}}
#' @export
#' @examples
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' x <- sqlite.build(filename = test.dat, db = test.sqlite, 'snp_test')
#' unlink(test.sqlite)
sqlite.build <- function(filename, db, table.name, new.colnames = NULL, overwrite = TRUE, 
  verbose = FALSE, ...) {
  info.msg(sprintf("Running sqlite.build function [filename:%s, db:%s, table.name:%s].", 
    filename, db, table.name), verbose = verbose)
  if (file.exists(db)) {
    if (overwrite) {
      info.msg(sprintf("overwrite be setted TRUE, removing %s", db), verbose = verbose)
      unlink(db)
    } else {
      warning(sprintf("%s already exists.", db))
      return(FALSE)
    }
  }
  status <- FALSE
  info.msg(sprintf("Setting up connection: %s sqlite databse.", db), verbose = verbose)
  sqlite.db <- dbConnect(RSQLite::SQLite(), db)
  info.msg(sprintf("Reading file %s.", filename), verbose = verbose)
  dat <- fread(input = filename, ...)
  info.msg(sprintf("Default table colnames is %s.", paste0(colnames(dat), collapse = ", ")), 
    verbose = verbose)
  if (!is.null(new.colnames)) {
    colnames(dat) <- new.colnames
    info.msg(sprintf("Table colnames be setted to %s.", paste0(colnames(dat), 
      collapse = ", ")), verbose = verbose)
  }
  info.msg(sprintf("Writing table %s in %s sqlite database.", table.name, db), 
    verbose = verbose)
  status <- dbWriteTable(sqlite.db, table.name, dat)
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", db), 
    verbose = verbose)
  dbDisconnect(sqlite.db)
  return(status)
}

#' Generate search index of annovarR database table in sqlite
#'
#' @param db Output path of sqlite database
#' @param table.name Table name in sqlite 
#' @param index Index name in sqlite 
#' @param cols Colnames needed to be index
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters be used in \code{\link[data.table]{fread}}
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, db = test.sqlite, table.name = 'snp_test')
#' x <- sqlite.index(db = test.sqlite, table.name = 'snp_test', index = 'index4', cols = c('V1', 'V2'))
#' unlink(test.sqlite)
sqlite.index <- function(db, table.name, index, cols, verbose = FALSE) {
  info.msg(sprintf("Running sqlite.index function [db:%s, table.name:%s, index:%s, cols:%s].", 
    db, table.name, index, paste0(cols, collapse = "; ")), verbose = verbose)
  info.msg(sprintf("Setting up connection: %s sqlite databse.", db), verbose = verbose)
  sqlite.db <- dbConnect(RSQLite::SQLite(), db)
  sql <- "SELECT * FROM sqlite_master WHERE type = \"index\""
  indexs <- dbGetQuery(sqlite.db, sql)
  if (index %in% indexs) {
    info.msg(sprintf("%s index already exists.", index), verbose)
    return(FALSE)
  }
  cols <- paste0(cols, collapse = "', '")
  cols <- paste0("'", cols, "'")
  sql <- sprintf("CREATE INDEX '%s' ON '%s' (%s)", index, table.name, cols)
  status <- FALSE
  info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
  status <- dbSendQuery(sqlite.db, sql)
  print.vb(status, verbose)
  info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
  dbClearResult(status)
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", db), 
    verbose = verbose)
  dbDisconnect(sqlite.db)
  return(status)
}

#' Drop search index of annovarR database table in sqlite
#'
#' @param db Output path of sqlite database
#' @param index Index name in sqlite 
#' @param verbose Ligical indicating wheather show the log message
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, db = test.sqlite, table.name = 'snp_test')
#' x <- sqlite.index(db = test.sqlite, table.name = 'snp_test', index = 'index4', cols = c('V1', 'V2'))
#' x <- drop.sqlite.index(db = test.sqlite, index = 'index4')
#' unlink(test.sqlite)
drop.sqlite.index <- function(db, index, verbose = FALSE) {
  info.msg(sprintf("Running drop.sqlite.index function [db:%s, index:%s].", db, 
    index), verbose = verbose)
  status <- FALSE
  info.msg(sprintf("Setting up connection: %s sqlite databse.", db), verbose = verbose)
  sqlite.db <- dbConnect(RSQLite::SQLite(), db)
  sql <- "SELECT * FROM sqlite_master WHERE type = 'index'"
  indexs <- dbGetQuery(sqlite.db, sql)
  if (!(index %in% indexs)) {
    info.msg(sprintf("%s index not existed in %s sqlite database.", index, db), 
      verbose)
    return(FALSE)
  }
  sql <- sprintf("DROP INDEX '%s'", index)
  info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
  status <- dbSendQuery(sqlite.db, sql)
  print.vb(status, verbose)
  info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
  dbClearResult(status)
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", db), 
    verbose = verbose)
  dbDisconnect(sqlite.db)
  return(status)
}
