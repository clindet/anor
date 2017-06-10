#' Build annovarR database in sqlite
#'
#' @param filename Path of raw data, will be read by fread
#' @param sqlite.connect.params Connect to sqlite database params [sqlite.path, table.name]
#' @param dat Object of data.table, as the input data to build database (optional)
#' @param fread.params Other parameters be used in \code{\link[data.table]{fread}}
#' @param new.colnames New colnames of table, default is to retain the original
#' @param overwrite Ligical indicating wheather overwrite sqlite database
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters be used in dbWriteTable
#' @export
#' @examples
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' x <- sqlite.build(filename = test.dat, list(sqlite.path = test.sqlite, 
#' table.name = 'snp_test'))
#' test.sqlite <- normalizePath(test.sqlite, '/')
#' file.remove(test.sqlite)
sqlite.build <- function(filename = "", sqlite.connect.params = list(sqlite.path = "", 
  table.name = ""), dat = data.table(), fread.params = list(), new.colnames = NULL, 
  overwrite = TRUE, verbose = FALSE, ...) {
  sqlite.path <- sqlite.connect.params[["sqlite.path"]]
  if (names(sqlite.connect.params)[1] != "") {
    sqlite.connect.params <- config.list.merge(list(sqlite.connect.params[["sqlite.path"]]), 
      sqlite.connect.params)
  }
  table.name <- sqlite.connect.params[["table.name"]]
  status <- FALSE
  if (filename != "") {
    info.msg(sprintf("Running sqlite.build function [filename:%s, sqlite.path:%s, table.name:%s].", 
      filename, sqlite.path, table.name), verbose = verbose)
    if (file.exists(sqlite.path)) {
      if (overwrite) {
        info.msg(sprintf("overwrite be setted TRUE, removing %s", sqlite.path), 
          verbose = verbose)
        file.remove(sqlite.path)
      } else {
        warning(sprintf("%s already exists.", sqlite.path))
        return(FALSE)
      }
    }
    info.msg(sprintf("Reading file %s.", filename), verbose = verbose)
    fread.params <- config.list.merge(list(input = filename), fread.params)
    dat <- do.call(fread, fread.params)
  } else if (length(dat) > 0) {
    info.msg(sprintf("Running sqlite.build function [sqlite.path:%s, table.name:%s], using `data` input.", 
      sqlite.path, table.name), verbose = verbose)
  } else {
    stop("Please set filenmae or dat at least one!")
  }
  
  info.msg(sprintf("Setting up connection: %s sqlite databse.", sqlite.path), verbose = verbose)
  sqlite.connect.params <- config.list.merge(list(SQLite()), sqlite.connect.params)
  sqlite.db <- do.call(dbConnect, sqlite.connect.params)
  info.msg(sprintf("Default table colnames is %s.", paste0(colnames(dat), collapse = ", ")), 
    verbose = verbose)
  if (!is.null(new.colnames)) {
    colnames(dat) <- new.colnames
    info.msg(sprintf("Table colnames be setted to %s.", paste0(colnames(dat), 
      collapse = ", ")), verbose = verbose)
  }
  info.msg(sprintf("Writing table %s in %s sqlite database.", table.name, sqlite.path), 
    verbose = verbose)
  status <- dbWriteTable(sqlite.db, table.name, dat, ...)
  print.vb(status, verbose = verbose)
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", sqlite.path), 
    verbose = verbose)
  dbDisconnect(sqlite.db)
  return(status)
}

#' Generate search index of annovarR database table in sqlite
#'
#' @param sqlite.connect.params Connect to sqlite database params [sqlite.path, table.name]
#' @param index Index name in sqlite 
#' @param cols Colnames needed to be index
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters be used in dbSendQuery
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, list(sqlite.path = test.sqlite, 
#' table.name = 'snp_test'))
#' x <- sqlite.index(list(sqlite.path = test.sqlite, table.name = 'snp_test'), 
#' index = 'index4', cols = c('V1', 'V2'))
#' test.sqlite <- normalizePath(test.sqlite, '/')
#' file.remove(test.sqlite)
sqlite.index <- function(sqlite.connect.params = list(sqlite.path = "", table.name = ""), 
  index = "", cols = c(), verbose = FALSE, ...) {
  sqlite.path <- sqlite.connect.params[["sqlite.path"]]
  if (names(sqlite.connect.params)[1] != "") {
    sqlite.connect.params <- config.list.merge(list(sqlite.connect.params[["sqlite.path"]]), 
      sqlite.connect.params)
  }
  table.name <- sqlite.connect.params[["table.name"]]
  info.msg(sprintf("Running sqlite.index function [sqlite.path:%s, table.name:%s, index:%s, cols:%s].", 
    sqlite.path, table.name, index, paste0(cols, collapse = "; ")), verbose = verbose)
  info.msg(sprintf("Setting up connection: %s sqlite databse.", sqlite.path), verbose = verbose)
  sqlite.connect.params <- config.list.merge(list(SQLite()), sqlite.connect.params)
  sqlite.db <- do.call(dbConnect, sqlite.connect.params)
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
  status <- dbSendQuery(sqlite.db, sql, ...)
  print.vb(status, verbose)
  info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
  dbClearResult(status)
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", sqlite.path), 
    verbose = verbose)
  dbDisconnect(sqlite.db)
  return(status)
}

#' Drop search index of annovarR database table in sqlite
#'
#' @param sqlite.connect.params Connect to sqlite database params [sqlite.path]
#' @param index Index name in sqlite 
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters be used in dbSendQuery
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, list(sqlite.path = test.sqlite, 
#' table.name = 'snp_test'))
#' x <- sqlite.index(list(sqlite.path = test.sqlite, table.name = 'snp_test'), 
#' index = 'index4', cols = c('V1', 'V2'))
#' x <- drop.sqlite.index(list(sqlite.path = test.sqlite), index = 'index4')
#' test.sqlite <- normalizePath(test.sqlite, '/')
#' file.remove(test.sqlite)
drop.sqlite.index <- function(sqlite.connect.params = list(sqlite.path = "", table.name = ""), 
  index = "", verbose = FALSE, ...) {
  sqlite.path <- sqlite.connect.params[["sqlite.path"]]
  if (names(sqlite.connect.params)[1] != "") {
    sqlite.connect.params <- config.list.merge(list(sqlite.connect.params[["sqlite.path"]]), 
      sqlite.connect.params)
  }
  info.msg(sprintf("Running drop.sqlite.index function [sqlite.path:%s, index:%s].", 
    sqlite.path, index), verbose = verbose)
  status <- FALSE
  info.msg(sprintf("Setting up connection: %s sqlite databse.", sqlite.path), verbose = verbose)
  sqlite.connect.params <- config.list.merge(list(SQLite()), sqlite.connect.params)
  sqlite.db <- do.call(dbConnect, sqlite.connect.params)
  sql <- "SELECT * FROM sqlite_master WHERE type = 'index'"
  indexs <- dbGetQuery(sqlite.db, sql)
  if (!(index %in% indexs)) {
    info.msg(sprintf("%s index not existed in %s sqlite database.", index, sqlite.path), 
      verbose)
    return(FALSE)
  }
  sql <- sprintf("DROP INDEX '%s'", index)
  info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
  status <- dbSendQuery(sqlite.db, sql, ...)
  print.vb(status, verbose)
  info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
  dbClearResult(status)
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", sqlite.path), 
    verbose = verbose)
  dbDisconnect(sqlite.db)
  return(status)
}


#' Build annovarR database in mysql
#'
#' @param filename Path of raw data, will be read by fread
#' @param mysql.connect.params Mysql parameters, [host, dbname, table.name, user, password etc.]
#' @param dat Object of data.table, as the input data to build database
#' @param fread.params Other parameters be used in \code{\link[data.table]{fread}}
#' @param new.colnames New colnames of table, default is to retain the original
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters be used in dbWriteTable
#' @export
#' @examples
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' ##mysql.build(test.dat, list(host = 'host', dbname = 'db', 
#' ##table.name = 'table', user = 'user', password = 'password'))
mysql.build <- function(filename = "", mysql.connect.params = list(host = "", dbname = "", 
  table.name = "", user = "", password = ""), dat = data.table(), fread.params = list(), 
  new.colnames = NULL, verbose = FALSE, ...) {
  dbname <- mysql.connect.params[["dbname"]]
  table.name <- mysql.connect.params[["table.name"]]
  info.msg(sprintf("Running mysql.build function for %s.", filename), verbose = verbose)
  status <- FALSE
  if (filename != "") {
    info.msg(sprintf("Reading file %s.", filename), verbose = verbose)
    params <- list(input = filename)
    params <- config.list.merge(fread.params, params)
    dat <- do.call(fread, params)
  } else if (length(data.table) > 0) {
    info.msg(sprintf("Running mysql.build function [dbname:%s, table.name:%s], using `data` input.", 
      dbname, table.name), verbose = verbose)
  } else {
    stop("Please set filenmae or dat at least one!")
  }
  info.msg(sprintf("Setting up connection %s mysql databse.", dbname), verbose = verbose)
  mysql.connect.params <- config.list.merge(list(MySQL()), mysql.connect.params)
  mysql.db <- do.call(dbConnect, mysql.connect.params)
  info.msg(sprintf("Default table colnames is %s.", paste0(colnames(dat), collapse = ", ")), 
    verbose = verbose)
  if (!is.null(new.colnames)) {
    colnames(dat) <- new.colnames
    info.msg(sprintf("Table colnames be setted to %s.", paste0(colnames(dat), 
      collapse = ", ")), verbose = verbose)
  }
  info.msg(sprintf("Writing table %s in %s mysql database.", table.name, dbname), 
    verbose = verbose)
  status <- dbWriteTable(mysql.db, table.name, dat, ...)
  print.vb(status, verbose = verbose)
  info.msg(sprintf("Disconnect the connection with the %s mysql databse.", dbname), 
    verbose = verbose)
  dbDisconnect(mysql.db)
  return(status)
}

#' Generate search index of annovarR database table in mysql
#'
#' @param mysql.connect.params Mysql parameters, [host, dbname, table.name, user, password etc.]
#' @param index Index name in mysql 
#' @param cols Colnames needed with length to be index (e.g. c('V1(6)', 'V2'))
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters be used in dbSendQuery
#' @export
#' @examples
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' ##mysql.index(list(host = 'host', dbname = 'db', table.name = 'table', 
#' ##user = 'user', password = 'password'), index = 'index_name', cols = c('V1', 'V2'))
mysql.index <- function(mysql.connect.params = list(host = "", dbname = "", table.name = "", 
  user = "user", password = "password"), index = "", cols = c(), verbose = FALSE, 
  ...) {
  dbname <- mysql.connect.params[["dbname"]]
  table.name <- mysql.connect.params[["table.name"]]
  info.msg(sprintf("Running mysql.index function [dbname:%s, table.name:%s, index:%s, cols:%s].", 
    dbname, table.name, index, paste0(cols, collapse = "; ")), verbose = verbose)
  info.msg(sprintf("Setting up connection: %s mysql databse.", dbname), verbose = verbose)
  mysql.connect.params <- config.list.merge(list(MySQL()), mysql.connect.params)
  mysql.db <- do.call(dbConnect, mysql.connect.params)
  sql <- sprintf("SHOW INDEX FROM %s", table.name)
  indexs.info <- dbGetQuery(mysql.db, sql)
  indexs <- indexs.info$Key_name
  print.vb(indexs, verbose = verbose)
  if (index %in% indexs) {
    info.msg(sprintf("%s index already exists.", index), verbose)
    return(FALSE)
  }
  cols <- paste0(cols, collapse = ", ")
  sql <- sprintf("CREATE INDEX %s ON %s (%s)", index, table.name, cols)
  status <- FALSE
  info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
  status <- dbSendQuery(mysql.db, sql, ...)
  print.vb(status, verbose)
  info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
  dbClearResult(status)
  info.msg(sprintf("Disconnect the connection with the %s mysql databse.", dbname), 
    verbose = verbose)
  dbDisconnect(mysql.db)
  return(status)
}

#' Drop search index of annovarR database table in mysql
#'
#' @param mysql.connect.params Mysql parameters, [host, dbname, table.name, user, password etc.]
#' @param index Index name in mysql 
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters be used in dbSendQuery
#' @export
#' @examples
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' ##drop.mysql.index(list(host = 'host', dbname = 'db', user = 'user', password = 'password'), 
#' ##index = 'index_name')
drop.mysql.index <- function(mysql.connect.params = list(host = "", dbname = "", 
  table.name = "", user = "user", password = "password"), index = "", verbose = FALSE, 
  ...) {
  dbname <- mysql.connect.params[["dbname"]]
  table.name <- mysql.connect.params[["table.name"]]
  info.msg(sprintf("Running drop.mysql.index function [dbname:%s, index:%s].", 
    dbname, index), verbose = verbose)
  status <- FALSE
  info.msg(sprintf("Setting up connection: %s mysql databse.", dbname), verbose = verbose)
  mysql.connect.params <- config.list.merge(list(MySQL()), mysql.connect.params)
  mysql.db <- do.call(dbConnect, mysql.connect.params)
  sql <- sprintf("SHOW INDEX FROM %s", table.name)
  indexs.info <- dbGetQuery(mysql.db, sql)
  indexs <- indexs.info$Key_name
  if (!(index %in% indexs)) {
    info.msg(sprintf("%s index not existed in %s mysql database.", index, dbname), 
      verbose)
    return(FALSE)
  }
  sql <- sprintf("DROP INDEX %s on %s", index, table.name)
  info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
  status <- dbSendQuery(mysql.db, sql, ...)
  print.vb(status, verbose)
  info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
  dbClearResult(status)
  info.msg(sprintf("Disconnect the connection with the %s mysql databse.", dbname), 
    verbose = verbose)
  dbDisconnect(mysql.db)
  return(status)
}

#' Delete table or database (text file, sqlite, mysql)
#' @param filename Path of text format data
#' @param sqlite.connect.params Connect to sqlite database params [sqlite.path, table.name]
#' @param mysql.connect.params Mysql parameters, [host, dbname, table.name, user, password etc.]
#' @param del.type file (for filename), database (for sqlite and MySQL), table (for sqlite and MySQL) 
#' be supported (delete file, delete sqlite db file, delete table in sqlite db,
#' delete table in MySQL db and delete database in MySQL database)
#' @param db.type txt, sqlite or mysql
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters be used in dbSendQuery
#' @export
#' @examples
#' db <- tempfile()
#' file.create(db)
#' del(db, del.type = 'file')
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' x <- sqlite.build(filename = test.dat, list(sqlite.path = test.sqlite, 
#' table.name = 'snp_test'))
#' del(sqlite.connect.params = list(sqlite.path = test.sqlite, 
#' table.name = 'snp_test'), del.type = 'table')
#' del(sqlite.connect.params = list(sqlite.path = test.sqlite, 
#' table.name = 'snp_test'))
del <- function(filename = "", sqlite.connect.params = list(), mysql.connect.params = list(), 
  del.type = "database", db.type = "sqlite", verbose = FALSE, ...) {
  if (!del.type %in% c("file", "database", "table")) {
    stop("del.type needed: file, database or table")
  }
  status <- NULL
  if (filename != "" && del.type == "file") {
    info.msg(sprintf("File %s will be removed.", filename), verbose = verbose)
    filename <- normalizePath(filename, "/")
    status <- file.remove(filename)
  } else if (length(sqlite.connect.params) != 0) {
    sqlite.path <- sqlite.connect.params[["sqlite.path"]]
    if (names(sqlite.connect.params)[1] != "") {
      sqlite.connect.params <- config.list.merge(list(sqlite.connect.params[["sqlite.path"]]), 
        sqlite.connect.params)
    }
    if ((del.type == "database") || (del.type == "file")) {
      info.msg(sprintf("Sqlite database %s will be removed.", sqlite.path), 
        verbose = verbose)
      sqlite.path <- normalizePath(sqlite.path, "/")
      status <- file.remove(sqlite.path)
    } else if (del.type == "table") {
      sqlite.connect.params <- config.list.merge(list(SQLite()), sqlite.connect.params)
      sqlite.db <- do.call(dbConnect, sqlite.connect.params)
      status <- RSQLite::dbRemoveTable(sqlite.db, sqlite.connect.params[["table.name"]])
      dbDisconnect(sqlite.db)
    }
  } else if (length(mysql.connect.params) != 0) {
    if (del.type == "database") {
      mysql.connect.params <- config.list.merge(list(MySQL()), mysql.connect.params)
      mysql.db <- do.call(dbConnect, mysql.connect.params)
      sql <- sprintf("DROP DATABASE %s", mysql.connect.params[["dbname"]])
      info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
      status <- dbSendQuery(sqlite.db, sql, ...)
      print.vb(status, verbose)
      info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
      dbClearResult(status)
      status <- TRUE
    } else if (del.type == "table") {
      info.msg(sprintf("Table %s in %s MySQL database will be removed.", mysql.connect.params[["table.name"]], 
        mysql.connect.params[["dbname"]]), verbose = verbose)
      mysql.db <- do.call(dbConnect, mysql.connect.params)
      status <- RMySQL::dbRemoveTable(mysql.db, mysql.connect.params[["table.name"]])
    }
  }
  return(status)
}

#' Convert sql file to sqlite database
#' @param sql.file SQL file of sqlite database dumped
#' @param statements SQL statements split by ';\\n' (small dataset)
#' @param sqlite.path Path of output sqlite database
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters be used in dbSendQuery
#' @export
#' @examples
#' sql.file <- system.file('extdata', 'demo/hg19_avsnp147.sqlite.sql', package = 'annovarR')
#' out.sqlite <- tempfile()
#' sqlite <- Sys.which(c('sqlite3', 'sqlite'))
#' sqlite <- sqlite[sqlite != ''][1]
#' sqlite <- unname(sqlite)
#' if (!is.na(sqlite)) {
#'   sql2sqlite(sql.file = sql.file, sqlite.path = out.sqlite, 
#'   verbose = FALSE)
#'   unlink(out.sqlite)
#' }
#' statements <- paste0(readLines(sql.file), collapse = '\n')
#' sql2sqlite(statements = statements, sqlite.path = out.sqlite, 
#' verbose = FALSE)
#' unlink(out.sqlite)
sql2sqlite <- function(sql.file = "", statements = "", sqlite.path = "", verbose = FALSE, 
  ...) {
  out.sqlite <- sqlite.path
  if (sql.file != "") {
    info.msg(sprintf("Converting %s to %s sqlite database.", sql.file, out.sqlite), 
      verbose = verbose)
  } else {
    info.msg(sprintf("Converting sql to %s sqlite database.", out.sqlite), verbose = verbose)
  }
  if (statements != "") {
    con <- do.call(dbConnect, list(SQLite(), sqlite.path))
    statements <- str_split(statements, ";\n")[[1]]
    func <- function(line) {
      print.vb(line, verbose = verbose)
      print.vb(rs, verbose = verbose)
      rs <- dbSendQuery(con, line, ...)
      dbClearResult(rs)
    }
    sapply(statements, func)
    dbDisconnect(con)
  } else {
    sqlite <- Sys.which(c("sqlite3", "sqlite"))
    sqlite <- sqlite[sqlite != ""][1]
    sqlite <- unname(sqlite)
    if (is.na(sqlite)) {
      return(FALSE)
    }
    sqlite <- normalizePath(sqlite, "/")
    cmd <- sprintf("%s %s < %s", sqlite, out.sqlite, sql.file)
    info.msg(sprintf("Running CMD:%s", cmd), verbose = verbose)
    system2(sqlite, args = out.sqlite, stdin = sql.file)
  }
}
