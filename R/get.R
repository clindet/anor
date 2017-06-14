#' Show top n line of table of database in sqlite database
#'
#' @param sqlite.connect.params Connect to sqlite database params [dbname, table.name]
#' @param n n lines will be selected
#' @param extra.sql Extra sql statement
#' @param verbose Ligical indicating wheather show the log message
#' @param ... Other parameters pass to dbGetQuery
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, list(dbname = test.sqlite, 
#' table.name = 'snp_test'))
#' sqlite.head(list(dbname = test.sqlite, table.name = 'snp_test'))
sqlite.head <- function(sqlite.connect.params = list(dbname = "", table.name = ""), 
  n = 10, extra.sql = NULL, verbose = FALSE, ...) {
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
#' @param sqlite.connect.params Connect to sqlite database params [dbname, table.name]
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' x <- sqlite.build(filename = test.dat, list(dbname = test.sqlite, 
#' table.name = 'snp_test'))
#' sqlite.tb.colnames(list(dbname = test.sqlite, table.name = 'snp_test'))
sqlite.tb.colnames <- function(sqlite.connect.params = list(dbname = "", table.name = "")) {
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

#' Get sqlite table indexes
#'
#' @param sqlite.connect.params Connect to sqlite database params [dbname, table.name]
#' @export
#' @examples
#' test.sqlite <- sprintf('%s/snp.test.sqlite', tempdir())
#' test.dat <- system.file('extdata', 'demo/sqlite.dat.txt', package = 'annovarR')
#' params <- list(dbname = test.sqlite,
#' table.name = 'snp_test')
#' x <- sqlite.build(filename = test.dat, params)
#' x <- sqlite.index(params, index = 'index4', cols = c('V1', 'V2'))
#' indexes <- sqlite.tb.indexes(params)
#' test.sqlite <- normalizePath(test.sqlite, '/')
#' file.remove(test.sqlite)
sqlite.tb.indexes <- function(sqlite.connect.params = list(dbname = "", table.name = "")) {
  sqlite.db <- connect.db("", "sqlite", sqlite.connect.params, verbose = FALSE)
  sql <- "SELECT * FROM sqlite_master WHERE type = 'index'"
  indexes <- dbGetQuery(sqlite.db, sql)
  dbDisconnect(sqlite.db)
  return(indexes)
}

# Show colnames of table in database or text file
db.tb.colnames <- function(dbname = "", db.type = "sqlite", sqlite.connect.params = list(), 
  mysql.connect.params = list()) {
  if (db.type == "sqlite") {
    tb.colnames <- sqlite.tb.colnames(sqlite.connect.params)
  } else if (db.type == "txt") {
    table.dat <- fread(dbname, nrows = 1)
    tb.colnames <- colnames(table.dat)
  } else if (db.type == "mysql") {
    tb.colnames <- mysql.tb.colnames(mysql.connect.params)
  }
}

# select.dat.full.match.sqlite
select.dat.full.match.sqlite <- function(db, table.name, cols = c(), params = list(), 
  select.cols = "*", sql.operator = NULL, verbose = FALSE) {
  params <- lapply(params, function(x) {
    as.character(x)
  })
  params.length <- length(params)
  if (is.null(sql.operator)) {
    sql.operator <- rep("==", length(params))
  }
  sql <- sprintf("SELECT %s FROM \"%s\"", select.cols, table.name)
  if (length(cols) > 0) {
    sql <- paste0(sql, " WHERE ")
    for (i in 1:params.length) {
      if (i < params.length) {
        sql.plus <- sprintf("\"%s\"%s:x%s AND ", cols[i], sql.operator[i], 
        i)
        sql <- paste0(sql, sql.plus)
      } else {
        sql.plus <- sprintf("\"%s\"%s:x%s", cols[i], sql.operator[i], i)
        sql <- paste0(sql, sql.plus)
      }
    }
  }
  info.msg(sprintf("Input %s colnum type:%s", paste0(names(params), collapse = ","), 
    paste0(sapply(params, typeof), collapse = ",")), verbose = verbose)
  print.vb(lapply(params, head), verbose = verbose)
  params <- unname(params)
  info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
  result <- dbGetQuery(db, sql, params = params)
  info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
  result <- as.data.table(result)
}

# select.dat.full.match.mysql
select.dat.full.match.mysql <- function(db, table.name, cols = c(), params = list(), 
  select.cols = "*", sql.operator = NULL, verbose = FALSE) {
  params <- lapply(params, function(x) {
    as.character(x)
  })
  params.length <- length(params)
  sql <- sprintf("SELECT %s FROM %s", select.cols, table.name)
  if (length(cols) >= 0) {
    sql <- sprintf("SELECT %s FROM %s", select.cols, table.name)
    sql <- paste0(sql, " WHERE ")
    for (i in 1:length(params)) {
      if (i < length(params)) {
        tmp.pars <- paste0(params[[cols[i]]], collapse = "', '")
        tmp.pars <- sprintf("'%s'", tmp.pars)
        sql.plus <- sprintf("%s in (%s) AND ", cols[i], tmp.pars)
        sql <- paste0(sql, sql.plus)
      } else {
        tmp.pars <- paste0(params[[cols[i]]], collapse = "', '")
        tmp.pars <- sprintf("'%s'", tmp.pars)
        sql.plus <- sprintf("%s in (%s)", cols[i], tmp.pars)
        sql <- paste0(sql, sql.plus)
      }
    }
    info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
    result <- dbGetQuery(db, sql)
  } else {
    info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
    result <- dbGetQuery(db, sql)
    info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
  }
  result <- as.data.table(result)
}

# select.dat.full.match.txt
select.dat.full.match.txt <- function(db, table.name, cols = c(), params = list(), 
  select.cols = "*", sql.operator = NULL, verbose = FALSE) {
  result <- fread(db)
  result <- lapply(result, function(x) {
    as.character(x)
  })
  params <- lapply(params, function(x) {
    as.character(x)
  })
  result <- as.data.table(result)
  params <- as.data.table(params)
  index <- match(colnames(result), names(params))
  index <- index[!is.na(index)]
  colnames(result)[index] <- names(params)
  keys <- paste0(names(params), collapse = "\", \"")
  text <- sprintf("setkey(result, \"%s\")", keys)
  eval(parse(text = text))
  params <- as.data.table(params)
  keys <- paste0(names(params), collapse = "\", \"")
  text <- sprintf("setkey(params, \"%s\")", keys)
  eval(parse(text = text))
  result <- merge(result, params)
}
# Select data from text file, sqlite or mysql database cols: database colnames
# (Simultaneously satisfy the cols SQL conditions) used to match params: a list
# that record to match database using cols
select.dat.full.match <- function(db, table.name, cols = c(), params = list(), db.type = "sqlite", 
  select.cols = "*", sql.operator = NULL, verbose = FALSE) {
  params <- lapply(params, function(x) {
    as.character(x)
  })
  params.length <- length(params)
  if (is.null(sql.operator)) {
    sql.operator <- rep("==", length(params))
  }
  if (db.type == "sqlite") {
    result <- select.dat.full.match.sqlite(db, table.name, cols, params, select.cols, sql.operator, verbose)
  } else if (db.type == "mysql") {
    result <- select.dat.full.match.mysql(db, table.name, cols, params, select.cols, sql.operator, verbose)
  } else if (db.type == "txt") {
    result <- select.dat.full.match.txt(db, table.name, cols, params, select.cols, sql.operator, verbose)
  }
  return(result)
}

# Read GFF and BED file or database
read.region <- function(filename = "", region.type = "gff", sqlite.connect.params = NULL, mysql.connect.params = NULL, 
                        ...) {
  if (filename != "") {
    
    region.type <- get.region.filetype(filename)
    dat <- fread(filename, ...)
  }
  
  if (region.type == "gff") {
    if (str_detect(filename, "bed$")) {
      warning("Reading BED prefix file by GFF3 option")
    }
    all.names <- c("seqid", "source", "type", "start", "end", "score", "strand", 
      "phase", "attributes")
  } else if (region.type == "gtf") {
    all.names <- c("seqid", "source", "method", "start", "end", "score", "strand", 
      "phase", "groups")
  } else if (region.type == "bed") {
    if (str_detect(filename, "gff$")) {
      warning("Reading BED prefix file by GFF option")
    }
    all.names <- c("chr", "start", "end", "name", "score", "strand", "thick_start", 
      "thick_end", "item_rgb", "block_count", "block_sizes", "block_starts")
  }
  colnames(dat)[1:ncol(dat)] <- all.names[1:ncol(dat)]
  return(dat)
}

get.region.filetype <- function(filename, use.prefix = TRUE) {
  filename <- tolower(filename)
  if (use.prefix) {
    if (str_detect(filename, "bed$")) {
      return("bed")
    } else if (str_detect(filename, "gff$")) {
      return("gff")
    } else if (str_detect(filename, "gtf$")) {
      return("gtf")
    } else {
      return("other")
    }
  }
}
# region match version select.dat
select.dat.region.match <- function(db, table.name, cols = c(), params = list(), 
  db.type = "sqlite", select.cols = "*", sql.operator = NULL, verbose = FALSE) {
  return(NULL)
}
