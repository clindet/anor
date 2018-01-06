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
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param database.cfg Configuration file of annovarR databases infomation
#' @export
#' @examples
#' get.annotation.dbtype('avsnp147') 
get.annotation.dbtype <- function(anno.name = "", database.cfg = system.file("extdata", 
  "config/databases.toml", package = "annovarR")) {
  dbtype <- tryCatch(get.cfg.value.by.name(anno.name, database.cfg, key = "default.dbtype"), 
    error = function(e) {
      return("txt")
    })
  if (is.null(dbtype)) {
    dbtype <- tryCatch(get.cfg.value.by.name(anno.name, database.cfg, key = "default.dbtype", 
      coincident = TRUE)[1], error = function(e) {
      return("txt")
    })
  }
  return(dbtype)
}

#' Get annovarR annotation needed colnames according the anno.name
#'
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param database.cfg Configuration file of annovarR databases infomation
#' @export
#' @examples
#' get.annotation.dbtype('avsnp147') 
get.annotation.needcols <- function(anno.name = "", database.cfg = system.file("extdata", 
  "config/databases.toml", package = "annovarR")) {
  need.cols <- get.cfg.value.by.name(anno.name, database.cfg, key = "need.cols", 
    coincident = TRUE)
  return(need.cols)
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
#' \dontrun{
#' mysql.db.colnames(list(host = 'host', dbname = 'db', user = 'user', 
#' password = 'password', table.name = 'table'))
#' }
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

#' Get mysql table indexes
#'
#' @param mysql.connect.params Connect to mysql database params [dbname, table.name, host, user, password]
#' @export
#' @examples
#' NULL
mysql.tb.indexes <- function(mysql.connect.params = list(dbname = "", table.name = "")) {
  mysql.db <- connect.db("", "mysql", mysql.connect.params = mysql.connect.params, 
    verbose = FALSE)
  sql <- sprintf("SHOW INDEX FROM %s", mysql.connect.params$table.name)
  indexes <- dbGetQuery(mysql.db, sql)
  indexes <- as.data.frame(indexes)
  indexes <- indexes[!duplicated(indexes$Key_name), ]
  colnames(indexes)[3] <- "name"
  dbDisconnect(mysql.db)
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
select.dat.full.match.sqlite <- function(db = NULL, table.name, cols = c(), params = list(), 
  select.cols = "*", sql.operator = NULL, verbose = FALSE) {
  params <- lapply(params, function(x) {
    if (!is.character(x)) {
      as.character(x)
    } else {
      x
    }
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
  names(params) <- paste0("x", 1:length(params))
  info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
  result <- dbGetQuery(db, sql, params = params)
  info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
  as.data.table(result)
}

# select.dat.full.match.mysql
select.dat.full.match.mysql <- function(db = NULL, table.name, cols = c(), params = list(), 
  select.cols = "*", sql.operator = NULL, verbose = FALSE) {
  params <- lapply(params, function(x) {
    if (!is.character(x)) {
      as.character(x)
    } else {
      x
    }
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
  as.data.table(result)
}

# select.dat.full.match.txt
select.dat.full.match.txt <- function(db = NULL, table.name, cols = c(), params = list(), 
  select.cols = "*", sql.operator = NULL, fread.db.params = list(), verbose = FALSE) {
  fread.params <- list(input = db)
  if ("logical01" %in% formalArgs(fread)) {
    config.list.merge(fread.params, list(logical01 = FALSE))
  }
  fread.params <- config.list.merge(fread.params, fread.db.params)
  suppressWarnings(ref.dat <- do.call(fread, fread.params))
  ref.dat.colnames.raw <- colnames(ref.dat)
  ref.dat <- lapply(ref.dat, function(x) {
    if (!is.character(x)) {
      as.character(x)
    } else {
      x
    }
  })
  params <- lapply(params, function(x) {
    if (!is.character(x)) {
      as.character(x)
    } else {
      x
    }
  })
  ref.dat <- as.data.table(ref.dat)
  params <- as.data.table(params)
  index <- match(names(params), colnames(ref.dat))
  index <- index[!is.na(index)]
  colnames(ref.dat)[index] <- names(params)
  keys <- paste0(names(params), collapse = "\", \"")
  text <- sprintf("setkey(ref.dat, \"%s\")", keys)
  eval(parse(text = text))
  params <- as.data.table(params)
  keys <- paste0(names(params), collapse = "\", \"")
  text <- sprintf("setkey(params, \"%s\")", keys)
  eval(parse(text = text))
  ref.dat <- merge(params, ref.dat)
  index <- match(ref.dat.colnames.raw, colnames(ref.dat))
  setcolorder(ref.dat, index)
  return(ref.dat)
}
# Select data from text file, sqlite or mysql database cols: database colnames
# (Simultaneously satisfy the cols SQL conditions) used to match params: a list
# that record to match database using cols
select.dat.full.match <- function(db = NULL, table.name = NULL, cols = c(), params = list(), 
  db.type = "sqlite", select.cols = "*", sql.operator = NULL, fread.db.params = list(), 
  verbose = FALSE) {
  params.length <- length(params)
  if (is.null(sql.operator)) {
    sql.operator <- rep("==", length(params))
  }
  if (db.type == "sqlite") {
    result <- select.dat.full.match.sqlite(db, table.name, cols, params, select.cols, 
      sql.operator, verbose)
  } else if (db.type == "mysql") {
    result <- select.dat.full.match.mysql(db, table.name, cols, params, select.cols, 
      sql.operator, verbose)
  } else if (db.type == "txt") {
    result <- select.dat.full.match.txt(db, table.name, cols, params, select.cols, 
      sql.operator, fread.db.params, verbose)
  }
  return(result)
}

# Region match from txt file (eg. gff, gtf, bed)
select.dat.region.match.sqlite <- function(db = NULL, table.name = NULL, full.matched.cols = c(), 
  inferior.col = c(), superior.col = c(), params = list(), select.cols = "*", verbose = FALSE, 
  ...) {
  sql.operator <- c(rep("==", length(full.matched.cols)), "<=", ">=")
  params$superior.col <- params$inferior.col
  result <- select.dat.full.match.sqlite(db, table.name, c(full.matched.cols, inferior.col, 
    superior.col), params, select.cols, sql.operator, verbose)
  result <- result[!duplicated(result), ]
}
# Region match from txt file (eg. gff, gtf, bed)
select.dat.region.match.txt <- function(db = NULL, table.name = NULL, full.matched.cols = c(), 
  inferior.col = c(), superior.col = c(), params = list(), select.cols = "*", fread.db.params = list(), 
  verbose = FALSE, ...) {
  fread.params <- list(input = db)
  if ("logical01" %in% formalArgs(fread)) {
    config.list.merge(fread.params, list(logical01 = FALSE))
  }
  fread.params <- config.list.merge(fread.params, fread.db.params)
  suppressWarnings(ref.dat <- do.call(fread, fread.params))
  result.list <- full.foverlaps(ref.dat, params, full.matched.cols, inferior.col, 
    superior.col)
  ref.dat <- result.list$ref.dat
  index.table <- result.list$index.table
  index <- index.table$yid[!is.na(index.table$yid)]
  index <- index[!duplicated(index)]
  ref.dat <- ref.dat[index, ]
}

# Read GFF and BED file or database
select.dat.region.match <- function(db = NULL, table.name = NULL, full.matched.cols = c(), 
  inferior.col = c(), superior.col = c(), params = list(), db.type = "txt", select.cols = "*", 
  fread.db.params = list(), verbose = FALSE, ...) {
  params <- lapply(params, function(x) {
    if (!is.character(x)) {
      as.character(x)
    } else {
      x
    }
  })
  params.length <- length(params)
  if (db.type == "sqlite") {
    result <- select.dat.region.match.sqlite(db, table.name, full.matched.cols, 
      inferior.col, superior.col, params, select.cols, verbose)
  } else if (db.type == "txt") {
    result <- select.dat.region.match.txt(db, table.name, full.matched.cols, 
      inferior.col, superior.col, params, select.cols, fread.db.params, verbose)
  }
  return(result)
}

full.foverlaps <- function(ref.dat = NULL, input.dat = NULL, full.matched.cols = NULL, 
  inferior.col = NULL, superior.col = NULL) {
  ref.dat <- as.data.table(ref.dat)
  ref.dat.colnames.raw <- colnames(ref.dat)
  input.dat <- as.data.table(input.dat)
  index <- match(names(input.dat), colnames(ref.dat))
  index <- index[!is.na(index)]
  colnames(ref.dat)[index] <- names(input.dat)
  texts <- sprintf("ref.dat$%s <- as.numeric(ref.dat$%s)", inferior.col, inferior.col)
  texts <- c(texts, sprintf("input.dat$%s <- as.numeric(input.dat$%s)", inferior.col, 
    inferior.col))
  texts <- c(texts, sprintf("ref.dat$%s <- as.numeric(ref.dat$%s)", superior.col, 
    superior.col))
  texts <- c(texts, sprintf("input.dat$%s <- as.numeric(input.dat$%s)", superior.col, 
    superior.col))
  for (i in texts) {
    eval(parse(text = i))
  }
  keys <- paste0(names(input.dat), collapse = "\", \"")
  text <- sprintf("setkey(ref.dat, \"%s\")", keys)
  eval(parse(text = text))
  id <- 1:nrow(input.dat)
  keys <- paste0(names(input.dat), collapse = "\", \"")
  text <- sprintf("setkey(input.dat, \"%s\")", keys)
  input.dat <- cbind(input.dat, id)
  input.dat <- as.data.table(input.dat)
  eval(parse(text = text))
  index.table <- foverlaps(input.dat, ref.dat, type = "any", which = TRUE)
  index.table$xid <- input.dat$id[index.table$xid]
  setkey(index.table, "xid")
  return(list(ref.dat = ref.dat, input.dat = input.dat, index.table = index.table))
}
