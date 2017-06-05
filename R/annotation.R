#' A position annotation utils that can be used to write a yourself annotation function
#'
#' @param dat A data.table including all of your data, eg. data.table(chr=c(1,2,3), start=c(1111,1112,1113))
#' @param name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases (mysql no need)
#' @param db.col.order Using the index, you can rename the database table, and can be matched using matched.cols. 
#' @param index.col Using the selected cols to match data with sqlite database. eg. c('chr', 'start'), 'rs'
#' @param matched.cols Using the selected cols to match data with selected partial data by index.col limited.
#' @param return.col.index Setting the colnums need be returned
#' @param return.col.names Setting the returned colnum names
#' @param format.dat.fun A function to process input data. eg. as.numeric(dat$start); as.character(dat$chr)
#' @param dbname.fixed Database path (txt, sqlite) or name (MySQL), default is NULL, and get from setdb.fun 
#' (Set value will fix the dbname, and will be added in sqlite.connenct.params and mysql.connect.params)
#' @param table.name.fixed Table name, default is NULL, and get from set.table.fun (Set value will fix the table.name)
#' (Set value will fix the table.name, and will be added in sqlite.connenct.params and mysql.connect.params)
#' @param setdb.fun A function to process the name, buildver, database.dir and get the database path (MySQL return NULL)
#' @param set.table.fun A function to process the name, buildver and get the final table name
#' @param format.db.tb.fun A function to process the selected database table that can be used to matched with your data
#' @param db.type Setting the database type (sqlite, txt or mysql)
#' @param mysql.connect.params Connect MySQL database other parameters, 
#' e.g. list(host='11.11.11.1', port = '3306', user = '', password = '123456')
#' @param sqlite.connect.params Connect SqLite database other paramertes, default is not need
#' @param verbose Logical indicating wheather print the extra log infomation
#' @export
#' @examples
#' library(data.table)
#' chr <- c('chr1', 'chr2', 'chr1')
#' start <- c('10020', '10020', '10020')
#' end <- c('10020', '10020', '10020')
#' ref <- c('A', 'A', 'A')
#' alt <- c('-', '-', '-')
#' database <- system.file('extdata', 'demo/hg19_avsnp147.txt', package = 'annovarR')
#' database.dir <- dirname(database)
#' dat <- data.table(chr = chr, start = start, end = end, ref = ref, alt = alt)
#' x <- annotation.cols.match(dat, 'avsnp147', database.dir = database.dir, 
#' return.col.names = 'avSNP147', db.type = 'txt')
annotation.cols.match <- function(dat = data.table(), name = "", buildver = "hg19", 
  database.dir = Sys.getenv("annovarR_DB_DIR", NULL), db.col.order = 1:5, index.col = c("chr", 
    "start"), matched.cols = c("chr", "start", "end", "ref", "alt"), return.col.index = 6, 
  return.col.names = "", format.dat.fun = format.cols, dbname.fixed = NULL, table.name.fixed = NULL, 
  setdb.fun = set.db, set.table.fun = set.table, format.db.tb.fun = format.db.tb, 
  db.type = "sqlite", mysql.connect.params = list(), sqlite.connect.params = list(), 
  verbose = FALSE) {
  dat.names <- names(dat)
  if (is.null(database.dir) || database.dir == "") {
    stop("Parameter database.dir not be setted.")
  } else if (!dir.exists(database.dir)) {
    stop(sprintf("%s directory not existed.", database.dir))
  }
  info.msg(sprintf("Total %s line be prepared to be annotate with %s database.", 
    nrow(dat), name), verbose = verbose)
  print.vb(dat, verbose = verbose)
  info.msg("Formating the input data.", verbose = verbose)
  # format.dat.fun can standardize the input data
  dat <- format.dat.fun(dat)
  print.vb(dat, verbose = verbose)
  # dbname is path of sqlite or text database or is dbname of MySQL database
  if (is.null(dbname.fixed)) {
    setdb.fun.args <- methods::formalArgs(setdb.fun)
    setdb.fun.params <- list(name = name, buildver = buildver, database.dir = database.dir, 
      db.type = db.type)
    if ("mysql.connect.params" %in% setdb.fun.args) {
      setdb.fun.params <- config.list.merge(setdb.fun.params, list(mysql.connect.params = mysql.connect.params))
    }
    if ("sqlite.connect.params" %in% setdb.fun.args) {
      setdb.fun.params <- config.list.merge(setdb.fun.params, list(sqlite.connect.params = sqlite.connect.params))
    }
    dbname <- do.call(setdb.fun, setdb.fun.params)
  } else {
    dbname <- dbname.fixed
  }
  if (db.type != "mysql" && !file.exists(dbname)) {
    stop(sprintf("%s database not existed, please check the database dir or setdb.fun function again.", 
      dbname))
  }
  database <- dbname
  if (db.type != "mysql") {
    info.msg(sprintf("Database path:%s", dbname))
  } else {
    info.msg(sprintf("Host: %s, Database:%s", mysql.connect.params$host, dbname))
  }
  if (is.null(table.name.fixed)) {
    set.table.fun.params <- list(name = name, buildver = buildver)
    table.name <- do.call(set.table.fun, set.table.fun.params)
  } else {
    table.name <- table.name.fixed
  }
  if (db.type == "mysql") {
    info.msg(sprintf("Setting up connection: Host:%s databse:%s.", mysql.connect.params$host, 
      mysql.connect.params$dbname), verbose = verbose)
  } else {
    info.msg(sprintf("Setting up connection: %s databse.", dbname), verbose = verbose)
  }
  database <- connect.db(database, db.type, sqlite.connect.params, mysql.connect.params)
  tb.colnames <- db.tb.colnames(dbname, table.name, db.type, mysql.connect.params)
  info.msg("Database colnames:%s", paste0(tb.colnames, collapse = ", "), verbose = verbose)
  dup <- !duplicated(dat)
  params = dat[dup, index.col, with = FALSE]
  index.col.order <- match(colnames(dat), index.col)
  index.col.order <- index.col.order[!is.na(index.col.order)]
  colnames(params) <- tb.colnames[index.col.order]
  info.msg(sprintf("After drop duplicated, %s colnum total %s line be used to select dat from database (%s).", 
    paste0(index.col, collapse = ","), nrow(params), paste0(names(params), collapse = ",")), 
    verbose = verbose)
  print.vb(params, verbose = verbose)
  selected.db.tb <- select.dat(database, table.name, tb.colnames[index.col.order], 
    params = params, db.type = db.type, verbose = verbose)
  selected.db.tb <- format.db.tb.fun(selected.db.tb)
  info.msg(sprintf("Total %s line be selected from database:", nrow(selected.db.tb)), 
    verbose = verbose)
  print.vb(selected.db.tb, verbose = verbose)
  if (nrow(selected.db.tb) == 0) {
    empty.col <- return.empty.col(dat, tb.colnames, return.col.index, return.col.names)
    return(empty.col)
  }
  selected.db.tb <- sync.colnames(selected.db.tb, db.col.order, dat.names)
  tb.colnames <- colnames(selected.db.tb)
  info.msg(sprintf("After sync colnames, the selected data colnames:%s", paste0(tb.colnames, 
    collapse = ",")), verbose = verbose)
  selected.colnames <- tb.colnames[return.col.index]
  id <- 1:nrow(dat)
  dat <- cbind(dat, id)
  dat <- as.data.table(lapply(dat, function(x) as.character(x)))
  selected.db.tb <- as.data.table(lapply(selected.db.tb, function(x) as.character(x)))
  keys <- paste0(matched.cols, collapse = ",")
  text <- sprintf("setkey(dat, %s)", keys)
  eval(paste0(text = text))
  text <- sprintf("setkey(selected.db.tb, %s)", keys)
  eval(paste0(text = text))
  selected.db.tb <- merge(selected.db.tb, dat, all = TRUE)
  selected.db.tb$id <- as.numeric(selected.db.tb$id)
  setkey(selected.db.tb, id)
  selected.db.tb <- selected.db.tb[!is.na(selected.db.tb$id), ]
  selected.db.tb <- selected.db.tb[!duplicated(selected.db.tb$id), ]
  info.msg(sprintf("Total %s line be processed.", nrow(selected.db.tb)), verbose = verbose)
  info.msg(sprintf("Matched data using %s colnums %s:", paste0(matched.cols, collapse = ",")))
  print.vb(selected.db.tb, verbose = verbose)
  selected.db.tb <- selected.db.tb[, selected.colnames, with = FALSE]
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", dbname), 
    verbose = verbose)
  disconnect.db(database, db.type)
  result <- selected.db.tb
  if (return.col.names != "") {
    colnames(result) <- return.col.names
  } else {
    colnames(result) <- tb.colnames[return.col.index]
  }
  info.msg("Returned data:", verbose = verbose)
  print.vb(result, verbose = verbose)
  return(result)
}

#' Annotation function (single name)
#'
#' @param dat A data.table including all of your data, eg. data.table(chr=c(1,2,3), start=c(1111,1112,1113))
#' @param name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all .etc.
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases
#' @param db.type Setting the database type (sqlite or txt)
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param func Function to anntate the dat data, default is to search the function in extdata/database.toml
#' @param mysql.connect.params Connect MySQL database other parameters, 
#' e.g. list(host='11.11.11.1', port = '3306', user = '', password = '123456')
#' @param sqlite.connect.params Connect SqLite database other paramertes, default is not need
#' @param ... Other parametes see \code{\link{annotation.cols.match}}
#' @export
#' @examples
#' library(data.table)
#' chr <- c('chr1', 'chr2', 'chr1')
#' start <- c('10020', '10020', '10020')
#' end <- c('10020', '10020', '10020')
#' ref <- c('A', 'A', 'A')
#' alt <- c('-', '-', '-')
#' database <- system.file('extdata', 'demo/hg19_avsnp147.txt', package = 'annovarR')
#' database.dir <- dirname(database)
#' dat <- data.table(chr = chr, start = start, end = end, ref = ref, alt = alt)
#' x <- annotation(dat, 'avsnp147', database.dir = database.dir, 
#' return.col.names = 'avSNP147', db.type = 'txt')
annotation <- function(dat = data.table(), name = "", buildver = "hg19", database.dir = Sys.getenv("annovarR_DB_DIR", 
  ""), db.type = NULL, database.cfg = system.file("extdata", "config/databases.toml", 
  package = "annovarR"), func = NULL, mysql.connect.params = list(host = "", dbname = "", 
  table.name = "", user = "", password = ""), sqlite.connect.params = list(sqlite.path = "", 
  table.name = ""), ...) {
  result <- NULL
  if (is.null(db.type)) {
    db.type <- get.annotation.dbtype(name, database.cfg = database.cfg)
  }
  if (is.null(func)) {
    func <- get.annotation.func(name, database.cfg = database.cfg)
    func <- eval(parse(text = func))
  }
  params <- list(dat = dat, name = name, buildver = buildver, database.dir = database.dir, 
    db.type = db.type, mysql.connect.params = mysql.connect.params, sqlite.connect.params = sqlite.connect.params, 
    ...)
  result <- do.call(func, params)
  return(result)
}

#' Annotation function (mulitple name)
#'
#' @param names Annotation names, eg. c('avsnp138', 'avsnp147', '1000g2015aug_all')
#' @param ... Other parametes see \code{\link{annotation}}
#' @export
#' @examples
#' library(data.table)
#' chr <- c('chr1', 'chr2', 'chr1')
#' start <- c('10020', '10020', '10020')
#' end <- c('10020', '10020', '10020')
#' ref <- c('A', 'A', 'A')
#' alt <- c('-', '-', '-')
#' database <- system.file('extdata', 'demo/hg19_avsnp147.txt', package = 'annovarR')
#' database.dir <- dirname(database)
#' dat <- data.table(chr = chr, start = start, end = end, ref = ref, alt = alt)
#' x <- annotation.merge(dat = dat, names = c('avsnp147', '1000g2015aug_all'), 
#' database.dir = database.dir, db.type = 'txt')
annotation.merge <- function(names, ...) {
  result.list <- lapply(names, function(x) {
    annotation(name = x, ...)
  })
  return(as.data.table(result.list))
}

annotation.auto <- function(dat, name, return.col.names = NULL, return.col.index = NULL, 
  db.col.order = NULL, matched.cols = NULL, dbname.fixed = NULL, table.name.fixed = NULL, 
  setdb.fun = NULL, set.table.fun = NULL, format.db.tb.fun = NULL, database.cfg = system.file("extdata", 
    "config/databases.toml", package = "annovarR"), ...) {
  
  dat.need.names <- get.cfg.value.by.name(name, database.cfg, key = "need.cols", 
    coincident = TRUE, extra.list = list(name = name), rcmd.parse = TRUE)
  
  dat <- dat[, colnames(dat) %in% dat.need.names, with = FALSE]
  
  supported.auto.names <- get.annotation.names(database.cfg = database.cfg)
  if (!name %in% supported.auto.names) {
    stop(sprintf("%s not be supprted by annotation.auto, please check the name and %s.", 
      name, database.cfg))
  }
  
  auto.parameters <- c("return.col.names", "return.col.index", "db.col.order", 
    "matched.cols", "setdb.fun", "set.table.fun", "format.db.tb.fun")
  para.values <- list()
  for (item in auto.parameters) {
    item.value <- eval(parse(text = item))
    if (is.null(item.value)) {
      para.values[[item]] <- get.cfg.value.by.name(name, database.cfg, key = item, 
        coincident = TRUE, extra.list = list(name = name), rcmd.parse = TRUE)
    } else {
      para.values[[item]] <- item.value
    }
  }
  annotation.cols.match(dat = dat, name = name, return.col.names = para.values[["return.col.names"]], 
    return.col.index = para.values[["return.col.index"]], db.col.order = para.values[["db.col.order"]], 
    matched.cols = para.values[["matched.cols"]], setdb.fun = eval(parse(text = para.values[["setdb.fun"]])), 
    set.table.fun = eval(parse(text = para.values[["set.table.fun"]])), format.db.tb.fun = eval(parse(text = para.values[["format.db.tb.fun"]])), 
    dbname.fixed = dbname.fixed, table.name.fixed = table.name.fixed, ...)
}
