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
#' @param return.col.names.profix Setting the returned colnum names profix
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
#' @param fread.db.params For text format database, you can use fread.db.params to control the fread behavior
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
  return.col.names = "", return.col.names.profix = "", format.dat.fun = format.cols, 
  dbname.fixed = NULL, table.name.fixed = NULL, setdb.fun = set.db, set.table.fun = set.table, 
  format.db.tb.fun = format.db.tb, db.type = "sqlite", db.file.prefix = NULL, mysql.connect.params = list(), 
  sqlite.connect.params = list(), fread.db.params = list(), verbose = FALSE) {
  
  returned.list <- do.call(before.query.steps, list(dat = dat, anno.name = anno.name, 
    buildver = buildver, database.dir = database.dir, db.col.order = db.col.order, 
    index.cols = index.cols, matched.cols = matched.cols, format.dat.fun = format.dat.fun, 
    dbname.fixed = dbname.fixed, table.name.fixed = table.name.fixed, setdb.fun = setdb.fun, 
    set.table.fun = set.table.fun, db.type = db.type, db.file.prefix = db.file.prefix, 
    mysql.connect.params = mysql.connect.params, sqlite.connect.params = sqlite.connect.params, 
    verbose = verbose))
  dat <- returned.list$dat
  database.con <- returned.list$database.con
  table.name <- returned.list$table.name
  tb.colnames <- returned.list$tb.colnames
  index.cols.order <- returned.list$index.cols.order
  matched.cols.order <- returned.list$matched.cols.order
  dat.names <- returned.list$dat.names
  params <- returned.list$params
  rm(returned.list)
  gc()
  # Select data from database
  selected.db.tb <- select.dat.full.match(database.con, table.name, tb.colnames[index.cols.order], 
    params = params, db.type = db.type, fread.db.params = fread.db.params, verbose = verbose)
  return(do.call(after.query.steps, list(dat = dat, selected.db.tb = selected.db.tb, 
    format.db.tb.fun = format.db.tb.fun, return.col.index = return.col.index, 
    matched.cols = matched.cols, tb.matched.cols = tb.colnames[matched.cols.order], 
    db.col.order = db.col.order, return.col.names = return.col.names, return.col.names.profix = return.col.names.profix, 
    tb.colnames = tb.colnames, database.con = database.con, db.type = db.type, 
    dat.names = dat.names, get.final.table.fun = get.full.match.final.table, 
    query.type = "full", verbose = verbose)))
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
#' @param return.col.names.profix Setting the returned colnum names profix
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
#' @param fread.db.params For text format database, you can use fread.db.params to control the fread behavior
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
#' \dontrun{
#' x <- annotation.region.match(dat = dat, database.dir = tempdir(),
#' dbname.fixed = bed.sqlite, table.name.fixed = 'bed', 
#' db.type = 'sqlite', format.dat.fun = function(...) {
#' params = list(...);return(params[[1]])})
#' }
#' file.remove(bed.sqlite)
annotation.region.match <- function(dat = data.table(), anno.name = "", buildver = "hg19", 
  database.dir = Sys.getenv("annovarR_DB_DIR", ""), db.col.order = 1:3, index.cols = c("chr", 
    "start", "end"), full.matched.cols = "chr", inferior.col = "start", superior.col = "end", 
  return.col.index = 4, return.col.names = "", return.col.names.profix = "", format.dat.fun = format.cols, 
  dbname.fixed = NULL, table.name.fixed = NULL, setdb.fun = set.db, set.table.fun = set.table, 
  format.db.tb.fun = format.db.region.tb, db.type = "sqlite", db.file.prefix = NULL, 
  mysql.connect.params = list(), sqlite.connect.params = list(), fread.db.params = list(), 
  verbose = FALSE) {
  returned.list <- do.call(before.query.steps, list(dat = dat, anno.name = anno.name, 
    buildver = buildver, database.dir = database.dir, db.col.order = db.col.order, 
    index.cols = index.cols, matched.cols = c(full.matched.cols, inferior.col, 
      superior.col), format.dat.fun = format.dat.fun, dbname.fixed = dbname.fixed, 
    table.name.fixed = table.name.fixed, setdb.fun = setdb.fun, set.table.fun = set.table.fun, 
    db.type = db.type, db.file.prefix = db.file.prefix, mysql.connect.params = mysql.connect.params, 
    sqlite.connect.params = sqlite.connect.params, verbose = verbose))
  dat <- returned.list$dat
  database.con <- returned.list$database.con
  table.name <- returned.list$table.name
  tb.colnames <- returned.list$tb.colnames
  index.cols.order <- returned.list$index.cols.order
  dat.names <- returned.list$dat.names
  params <- returned.list$params
  dbname <- returned.list$params
  rm(returned.list)
  gc()
  
  # Sync matched colsnames with reference database
  full.matched.cols.raw <- full.matched.cols
  inferior.col.raw <- inferior.col
  superior.col.raw <- superior.col
  for (i in c("full.matched.cols", "inferior.col", "superior.col")) {
    index <- match(colnames(dat), eval(parse(text = i)))
    text <- sprintf("%s <- '%s'", i, colnames(params)[!is.na(index)])
    eval(parse(text = text))
  }
  
  # Select data from database
  select.params <- list(db = database.con, table.name = table.name, full.matched.cols = full.matched.cols, 
    inferior.col = inferior.col, superior.col = superior.col, params = params, 
    db.type = db.type, fread.db.params = fread.db.params, verbose = verbose)
  selected.db.tb <- do.call(select.dat.region.match, select.params)
  matched.cols = c(full.matched.cols.raw, inferior.col.raw, superior.col.raw)
  return(do.call(after.query.steps, list(dat = dat, selected.db.tb = selected.db.tb, 
    format.db.tb.fun = format.db.tb.fun, return.col.index = return.col.index, 
    full.matched.cols = full.matched.cols, full.matched.cols.raw = full.matched.cols.raw, 
    inferior.col = inferior.col, inferior.col.raw = inferior.col.raw, superior.col = superior.col, 
    superior.col.raw = superior.col.raw, db.col.order = db.col.order, params = params, 
    return.col.names = return.col.names, return.col.names.profix = return.col.names.profix, 
    tb.colnames = tb.colnames, database.con = database.con, db.type = db.type, 
    dat.names = dat.names, get.final.table.fun = get.region.match.final.table, 
    dbname = dbname, query.type = "region", verbose = verbose)))
}
#' Annotation function (single name)
#'
#' @param dat A data.table including all of your data, eg. data.table(chr=c(1,2,3), start=c(1111,1112,1113))
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all .etc.
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param annovar.anno.names If anno.name equal perl_annovar_merge, 
#' you can use annovar.anno.names to annotate multiple database supported by ANNOVAR,
#' the names can be found on the http://annovar.openbioinformatics.org/en/latest/user-guide/download/
#' @param database.dir Dir of the databases
#' @param db.type Setting the database type (sqlite or txt)
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param func Function to anntate the dat data, default is to search the function in extdata/database.toml
#' @param mysql.connect.params Connect MySQL database other parameters, 
#' e.g. list(host='11.11.11.1', port = '3306', user = '', password = '123456')
#' @param sqlite.connect.params Connect SqLite database other paramertes, default is not need
#' @param ... Other parameters see \code{\link{annotation.cols.match}}, \code{\link{annotation.region.match}}, 
#' \code{\link{annovar}} and \code{\link{vep}}
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
annotation <- function(dat = data.table(), anno.name = "", buildver = "hg19", annovar.anno.names = "", 
  database.dir = Sys.getenv("annovarR_DB_DIR", ""), db.type = NULL, database.cfg = system.file("extdata", 
    "config/databases.toml", package = "annovarR"), func = NULL, mysql.connect.params = list(host = "", 
    dbname = "", table.name = "", user = "", password = ""), sqlite.connect.params = list(dbname = ""), 
  ...) {
  if (length(anno.name) > 1) {
    stop("Length of anno.name > 1, please use annotation.merge.")
  }
  result <- NULL
  if (is.null(db.type)) {
    db.type <- get.annotation.dbtype(anno.name, database.cfg = database.cfg)
  }
  needcols <- get.annotation.needcols <- function(anno.name = anno.name, database.cfg = database.cfg) if (dim(dat) > 
    1 && colnames(dat)) {
    colnames(dat)[1:length(needcols)] <- needcols
  }
  if (is.null(func)) {
    func <- get.annotation.func(anno.name, database.cfg = database.cfg)
    func <- eval(parse(text = func))
  }
  params <- list(dat = dat, anno.name = anno.name, buildver = buildver, annovar.anno.names = annovar.anno.names, 
    database.dir = database.dir, db.type = db.type, mysql.connect.params = mysql.connect.params, 
    sqlite.connect.params = sqlite.connect.params, database.cfg = database.cfg, 
    ...)
  result <- do.call(func, params)
  return(result)
}

#' Annotation function (mulitple name)
#'
#' @param anno.names Annotation names, eg. c('avsnp138', 'avsnp147', '1000g2015aug_all')
#' @param ... Other parameters see \code{\link{annotation}}
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
#' x <- annotation.merge(dat = dat, anno.names = c('avsnp147'), 
#' database.dir = database.dir, db.type = 'txt')
annotation.merge <- function(anno.names, ...) {
  perl_annovar_names <- anno.names[str_detect(anno.names, "perl_annovar_")]
  result.list.1 <- lapply(anno.names[!anno.names %in% perl_annovar_names], function(x) {
    annotation(anno.name = x, ...)
  })
  if (length(perl_annovar_names) > 0) {
    result.list.2 <- lapply(perl_annovar_names, function(x) {
      y <- annotation(anno.name = x, ...)
      return(y)
    })
    names(result.list.2) <- perl_annovar_names
    if (length(result.list.1) != 0) {
      return(list(annovarR = as.data.table(result.list.1), annovar = result.list.2))
    } else {
      return(list(annovar = result.list.2))
    }
  } else {
    return(as.data.table(result.list.1))
  }
}
