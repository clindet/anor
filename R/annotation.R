#' A position annotation utils that can be used to write a yourself annotation function
#'
#' @param dat A data.table including all of your data, eg. data.table(chr=c(1,2,3), start=c(1111,1112,1113))
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases (mysql no need)
#' @param db.col.order Using the index, you can rename the database table, and can be matched using matched.cols. 
#' @param index.cols Using the selected cols to match data with sqlite database. eg. c('chr', 'start'), 'rs'
#' @param matched.cols Using the selected cols to match data with selected partial data by index.cols limited.
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
#' @param db.file.prefix Only be setted when db.type is local databae like sqlite or txt
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
annotation.cols.match <- function(dat = data.table(), anno.name = "", buildver = "hg19", 
  database.dir = Sys.getenv("annovarR_DB_DIR", ""), db.col.order = 1:5, index.cols = c("chr", 
    "start"), matched.cols = c("chr", "start", "end", "ref", "alt"), return.col.index = 6, 
  return.col.names = "", format.dat.fun = format.cols, dbname.fixed = NULL, table.name.fixed = NULL, 
  setdb.fun = set.db, set.table.fun = set.table, format.db.tb.fun = format.db.tb, 
  db.type = "sqlite", db.file.prefix = NULL, mysql.connect.params = list(), sqlite.connect.params = list(), 
  verbose = FALSE) {
  dat <- input.dat.check(dat)
  if (is.null(dat)) {
    return(NULL)
  }
  dat.names <- names(dat)
  database.dir.check(dbname.fixed = dbname.fixed, database.dir = database.dir)
  dat <- input.dat.initial(dat, format.dat.fun, verbose)
  db.file.prefix <- db.file.prefix.initial(db.type, db.file.prefix)
  # dbname is path of sqlite or text database or is dbname of MySQL database
  dbname <- dbname.initial(anno.name, dbname.fixed, setdb.fun, buildver, database.dir, 
    db.type, db.file.prefix, mysql.connect.params, sqlite.connect.params)
  # table.name initial
  table.name <- table.name.initial(anno.name, table.name.fixed, buildver, set.table.fun)
  
  # database.params initial
  database.params <- database.params.initial(db.type, dbname, table.name, sqlite.connect.params, 
    mysql.connect.params)
  sqlite.connect.params <- database.params[["sqlite"]]
  mysql.connect.params <- database.params[["mysql"]]
  print.db.info(dbname, db.type, mysql.connect.params, verbose)
  
  database.con <- connect.db(dbname, db.type, sqlite.connect.params, mysql.connect.params, 
    verbose)
  tb.colnames <- db.tb.colnames(dbname = dbname, db.type = db.type, sqlite.connect.params, 
    mysql.connect.params)
  tb.colnames.raw <- tb.colnames
  info.msg("Database colnames:%s", paste0(tb.colnames, collapse = ", "), verbose = verbose)
  
  print.vb(index.cols, verbose = verbose)
  print.vb(dat, verbose = verbose)
  # Get unique records, params is pass to select.dat.full.match and get matched
  # data table from database
  dup <- !duplicated(dat)
  params <- dat[dup, index.cols, with = FALSE]
  
  # Sync the colnames between input cols and database table cols which be used to
  # select data
  tb.colnames <- tb.colnames[db.col.order]
  index.cols.order <- match(colnames(dat), index.cols)
  index.cols.order <- index.cols.order[!is.na(index.cols.order)]
  colnames(params) <- tb.colnames[index.cols.order]
  info.msg(sprintf("After drop duplicated, %s colnum total %s line be used to select.dat.full.match from database (%s).", 
    paste0(index.cols, collapse = ","), nrow(params), paste0(names(params), collapse = ",")), 
    verbose = verbose)
  print.vb(params, verbose = verbose)
  
  # Select data from database
  selected.db.tb <- select.dat.full.match(database.con, table.name, tb.colnames[index.cols.order], 
    params = params, db.type = db.type, verbose = verbose)
  selected.db.tb <- format.db.tb.fun(db.tb = selected.db.tb, input.dat = dat)
  info.msg(sprintf("Total %s line be selected from database:", nrow(selected.db.tb)), 
    verbose = verbose)
  print.vb(selected.db.tb, verbose = verbose)
  
  # Check return.col.index, if empty return the all of cols in database without
  # matched cols
  if (all(return.col.index == "")) {
    all.cols <- 1:ncol(selected.db.tb)
    return.col.index <- all.cols[!all.cols %in% db.col.order]
  }
  
  # If selected data is empty, return NA matrix according the return.col.index and
  # return.col.names
  if (nrow(selected.db.tb) == 0) {
    empty.col <- return.empty.col(dat, tb.colnames, return.col.index, return.col.names)
    disconnect.db(database.con, db.type)
    return(empty.col)
  }
  
  # Sync colnames between selected data and input data
  selected.db.tb <- sync.colnames(selected.db.tb, db.col.order, dat.names)
  tb.colnames <- colnames(selected.db.tb)
  info.msg(sprintf("After sync colnames, the selected data colnames:%s", paste0(tb.colnames, 
    collapse = ",")), verbose = verbose)
  selected.colnames <- tb.colnames[return.col.index]
  
  selected.db.tb <- get.full.match.final.table(dat, selected.db.tb, matched.cols, 
    selected.colnames, verbose)
  
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", dbname), 
    verbose = verbose)
  disconnect.db(database.con, db.type)
  
  # Final process on result
  result <- selected.db.tb
  if (any(return.col.names != "")) {
    colnames(result) <- return.col.names
  } else {
    colnames(result) <- tb.colnames[return.col.index]
  }
  info.msg("Returned data:", verbose = verbose)
  print.vb(result, verbose = verbose)
  return(result)
}

#' A regeion annotation utils that can be used to write a yourself annotation function
#'
#' @param dat A data.table including all of your data, eg. data.table(chr=c(1,2,3), start=c(1111,1112,1113))
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases (mysql no need)
#' @param db.col.order Using the index, you can rename the database table, and can be matched using matched.cols. 
#' @param index.cols Using the selected cols to match data with sqlite database. eg. c('chr', 'start'), 'rs'
#' @param full.matched.cols Using the selected cols to match data with selected partial data by index.cols limited.
#' @param inferior.col Inferior limit col, e.g. start
#' @param superior.col Superior limit col, e.g. end
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
#' @param db.file.prefix Only be setted when db.type is local databae like sqlite or txt
#' @param mysql.connect.params Connect MySQL database other parameters, 
#' e.g. list(host='11.11.11.1', port = '3306', user = '', password = '123456')
#' @param sqlite.connect.params Connect SqLite database other paramertes, default is not need
#' @param verbose Logical indicating wheather print the extra log infomation
#' @export
#' @examples
#' library(data.table)
#' bed.file <- system.file('extdata', 'demo/example.bed', package = 'annovarR')
#' bed.sqlite <- sprintf('%s/%s.sqlite', tempdir(), basename(bed.file))
#' connect.params <- list(dbname = bed.sqlite, table.name = 'bed')
#' sqlite.build(bed.file, connect.params)
#' chr <- c('chr10', 'chr1')
#' start <- c('100188904', '100185955')
#' end <- c('100188904', '100185955')
#' dat <- data.table(chr = chr, start = start, end = end)
#' x <- annotation.region.match(dat = dat, database.dir = tempdir(),
#' dbname.fixed = bed.sqlite, table.name.fixed = 'bed', 
#' db.type = 'sqlite', format.dat.fun = function(x) return(x))
#' file.remove(bed.sqlite)
annotation.region.match <- function(dat = data.table(), anno.name = "", buildver = "hg19", 
  database.dir = Sys.getenv("annovarR_DB_DIR", ""), db.col.order = 1:3, index.cols = c("chr", 
    "start", "end"), full.matched.cols = "chr", inferior.col = "start", superior.col = "end", 
  return.col.index = 4, return.col.names = "", format.dat.fun = format.cols, dbname.fixed = NULL, 
  table.name.fixed = NULL, setdb.fun = set.db, set.table.fun = set.table, format.db.tb.fun = format.db.region.tb, 
  db.type = "sqlite", db.file.prefix = NULL, mysql.connect.params = list(), sqlite.connect.params = list(), 
  verbose = FALSE) {
  dat <- input.dat.check(dat)
  if (is.null(dat)) {
    return(NULL)
  }
  dat.names <- names(dat)
  database.dir.check(dbname.fixed = dbname.fixed, database.dir = database.dir)
  dat <- input.dat.initial(dat, format.dat.fun, verbose)
  db.file.prefix <- db.file.prefix.initial(db.type, db.file.prefix)

  # dbname is path of sqlite or text database or is dbname of MySQL database
  dbname <- dbname.initial(anno.name, dbname.fixed, setdb.fun, buildver, database.dir, 
    db.type, db.file.prefix, mysql.connect.params, sqlite.connect.params)
  
  # table.name initial
  table.name <- table.name.initial(anno.name, table.name.fixed, buildver, set.table.fun)
  
  # database.params initial
  database.params <- database.params.initial(db.type, dbname, table.name, sqlite.connect.params, 
    mysql.connect.params)
  sqlite.connect.params <- database.params[["sqlite"]]
  mysql.connect.params <- database.params[["mysql"]]
  print.db.info(dbname, db.type, mysql.connect.params, verbose)
  
  database.con <- connect.db(dbname, db.type, sqlite.connect.params, mysql.connect.params, 
    verbose)
  tb.colnames <- db.tb.colnames(dbname = dbname, db.type = db.type, sqlite.connect.params, 
    mysql.connect.params)
  info.msg("Database colnames:%s", paste0(tb.colnames, collapse = ", "), verbose = verbose)
  
  # Get unique records, params is pass to select.dat.full.match and get matched
  # data table from database
  dup <- !duplicated(dat)
  params <- dat[dup, index.cols, with = FALSE]
  
  # Sync the colnames between input cols and database table cols which be used to
  # select data
  tb.colnames <- tb.colnames[db.col.order]
  index.cols.order <- match(colnames(dat), index.cols)
  index.cols.order <- index.cols.order[!is.na(index.cols.order)]
  colnames(params) <- tb.colnames[index.cols.order]
  full.matched.cols.raw <- full.matched.cols
  inferior.col.raw <- inferior.col
  superior.col.raw <- superior.col
  for (i in c("full.matched.cols", "inferior.col", "superior.col")) {
    index <- match(colnames(dat), eval(parse(text = i)))
    text <- sprintf("%s <- '%s'", i, colnames(params)[!is.na(index)])
    eval(parse(text = text))
  }
  info.msg(sprintf("After drop duplicated, %s colnum total %s line be used to select.dat.region.match from database (%s).", 
    paste0(index.cols, collapse = ","), nrow(params), paste0(names(params), collapse = ",")), 
    verbose = verbose)
  print.vb(params, verbose = verbose)
  
  # Select data from database
  select.params <- list(db = database.con, table.name = table.name, full.matched.cols = full.matched.cols, 
    inferior.col = inferior.col, superior.col = superior.col, params = params, 
    db.type = db.type, verbose = verbose)
  selected.db.tb <- do.call(select.dat.region.match, select.params)
  selected.db.tb <- format.db.tb.fun(db.tb = selected.db.tb, input.dat = params, 
    inferior.col = inferior.col, superior.col = superior.col)
  info.msg(sprintf("Total %s line be selected from database:", nrow(selected.db.tb)), 
    verbose = verbose)
  print.vb(selected.db.tb, verbose = verbose)
  
  # Check return.col.index, if empty return the all of cols in database without
  # matched cols
  if (all(return.col.index == "")) {
    all.cols <- 1:ncol(selected.db.tb)
    return.col.index <- all.cols[!all.cols %in% db.col.order]
  }
  
  # If selected data is empty, return NA matrix according the return.col.index and
  # return.col.names
  if (nrow(selected.db.tb) == 0) {
    empty.col <- return.empty.col(dat, tb.colnames, return.col.index, return.col.names)
    disconnect.db(database.con, db.type)
    return(empty.col)
  }
  
  # Sync colnames between selected data and input data
  selected.db.tb <- sync.colnames(selected.db.tb, db.col.order, dat.names)
  tb.colnames <- colnames(selected.db.tb)
  info.msg(sprintf("After sync colnames, the selected data colnames:%s", paste0(tb.colnames, 
    collapse = ",")), verbose = verbose)
  selected.colnames <- tb.colnames[return.col.index]
  
  selected.db.tb <- get.region.match.final.table(dat, selected.db.tb, inferior.col.raw, 
    superior.col.raw, selected.colnames, verbose)
  
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", dbname), 
    verbose = verbose)
  disconnect.db(database.con, db.type)
  
  # Final process on result
  result <- selected.db.tb
  if (any(return.col.names != "")) {
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
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all .etc.
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
annotation <- function(dat = data.table(), anno.name = "", buildver = "hg19", database.dir = Sys.getenv("annovarR_DB_DIR", 
  ""), db.type = NULL, database.cfg = system.file("extdata", "config/databases.toml", 
  package = "annovarR"), func = NULL, mysql.connect.params = list(host = "", dbname = "", 
  table.name = "", user = "", password = ""), sqlite.connect.params = list(dbname = ""), 
  ...) {
  result <- NULL
  if (is.null(db.type)) {
    db.type <- get.annotation.dbtype(anno.name, database.cfg = database.cfg)
  }
  if (is.null(func)) {
    func <- get.annotation.func(anno.name, database.cfg = database.cfg)
    func <- eval(parse(text = func))
  }
  params <- list(dat = dat, anno.name = anno.name, buildver = buildver, database.dir = database.dir, 
    db.type = db.type, mysql.connect.params = mysql.connect.params, sqlite.connect.params = sqlite.connect.params, 
    ...)
  result <- do.call(func, params)
  return(result)
}

#' Annotation function (mulitple name)
#'
#' @param anno.names Annotation names, eg. c('avsnp138', 'avsnp147', '1000g2015aug_all')
#' @param col.cl.num Number of cores (default is NULL, not use cluster)
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
#' x <- annotation.merge(dat = dat, anno.names = c('avsnp147', '1000g2015aug_all'), 
#' database.dir = database.dir, db.type = 'txt')
annotation.merge <- function(anno.names, col.cl.num = NULL, ...) {
  if (is.null(col.cl.num)) {
    result.list <- lapply(anno.names, function(x) {
      annotation(anno.name = x, ...)
    })
  } else {
    if (col.cl.num > length(anno.names)) {
      col.cl.num <- length(anno.names)
    }
    col.cl <- makeCluster(col.cl.num)
    params <- list(...)
    dat <- params$dat
    clusterExport(col.cl, "dat", envir = environment())
    registerDoParallel(col.cl)
    result.list <- foreach(anno.names = anno.names, .combine = cbind, .packages = "annovarR") %dopar% 
      annotation(anno.name = anno.names, ...)
    stopCluster(col.cl)
  }
  return(as.data.table(result.list))
}
