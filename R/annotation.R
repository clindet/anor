#' A position annotation utils that can be used to write a yourself annotation function
#'
#' @param dat A data.table including all of your data, eg. data.table(chr=c(1,2,3), start=c(1111,1112,1113))
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases (mysql no need)
#' @param db_col_order Using the index, you can rename the database table, and can be matched using matched_cols. 
#' @param index_cols Using the selected cols to match data with sqlite database. eg. c('chr', 'start'), 'rs'
#' @param matched_cols Using the selected cols to match data with selected partial data by index_cols limited.
#' @param return_col_index Setting the colnums need be returned
#' @param return_col_names Setting the returned colnum names
#' @param return_col_names_profix Setting the returned colnum names profix
#' @param format_dat_fun A function to process input data. eg. as.numeric(dat$start); as.character(dat$chr)
#' @param dbname_fixed Database path (txt, sqlite) or name (MySQL), default is NULL, and get from setdb_fun 
#' (Set value will fix the dbname, and will be added in sqlite.connenct.params and mysql.connect.params)
#' @param table_name_fixed Table name, default is NULL, and get from set_table_fun (Set value will fix the table.name)
#' (Set value will fix the table.name, and will be added in sqlite.connenct.params and mysql.connect.params)
#' @param setdb_fun A function to process the name, buildver, database.dir and get the database path (MySQL return NULL)
#' @param set_table_fun A function to process the name, buildver and get the final table name
#' @param format_db_tb_fun A function to process the selected database table that can be used to matched with your data
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
#' return_col_names = 'avSNP147', db.type = 'txt')
annotation.cols.match <- function(dat = data.table(), anno.name = "", buildver = "hg19", 
  database.dir = Sys.getenv("annovarR_DB_DIR", ""), db_col_order = 1:5, index_cols = c("chr", 
    "start"), matched_cols = c("chr", "start", "end", "ref", "alt"), return_col_index = 6, 
  return_col_names = "", return_col_names_profix = "", format_dat_fun = format.cols, 
  dbname_fixed = NULL, table_name_fixed = NULL, setdb_fun = set.db, set_table_fun = set.table, 
  format_db_tb_fun = format.db.tb, db.type = "sqlite", db.file.prefix = NULL, mysql.connect.params = list(), 
  sqlite.connect.params = list(), fread.db.params = list(), verbose = FALSE) {
  
  returned.list <- do.call(before.query.steps, list(dat = dat, anno.name = anno.name, 
    buildver = buildver, database.dir = database.dir, db_col_order = db_col_order, 
    index_cols = index_cols, matched_cols = matched_cols, format_dat_fun = format_dat_fun, 
    dbname_fixed = dbname_fixed, table_name_fixed = table_name_fixed, setdb_fun = setdb_fun, 
    set_table_fun = set_table_fun, db.type = db.type, db.file.prefix = db.file.prefix, 
    mysql.connect.params = mysql.connect.params, sqlite.connect.params = sqlite.connect.params, 
    verbose = verbose))
  dat <- returned.list$dat
  database.con <- returned.list$database.con
  table.name <- returned.list$table.name
  tb.colnames <- returned.list$tb.colnames
  tb.colnames.raw <- returned.list$tb.colnames.raw
  index_cols.order <- returned.list$index_cols.order
  matched_cols.order <- returned.list$matched_cols.order
  dat.names <- returned.list$dat.names
  params <- returned.list$params
  rm(returned.list)
  gc()
  # Select data from database
  selected.db.tb <- select.dat.full.match(database.con, table.name, tb.colnames[index_cols.order], 
    params = params, db.type = db.type, fread.db.params = fread.db.params, verbose = verbose)
  return(do.call(after.query.steps, list(dat = dat, selected.db.tb = selected.db.tb, 
    format_db_tb_fun = format_db_tb_fun, return_col_index = return_col_index, 
    matched_cols = matched_cols, tb.matched_cols = tb.colnames[matched_cols.order], 
    db_col_order = db_col_order, return_col_names = return_col_names, return_col_names_profix = return_col_names_profix, 
    tb.colnames = tb.colnames, tb.colnames.raw = tb.colnames.raw, database.con = database.con, 
    db.type = db.type, dat.names = dat.names, get.final.table.fun = get.full.match.final.table, 
    query.type = "full", verbose = verbose)))
}

#' A regeion annotation utils that can be used to write a yourself annotation function
#'
#' @param dat A data.table including all of your data, eg. data.table(chr=c(1,2,3), start=c(1111,1112,1113))
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases (mysql no need)
#' @param db_col_order Using the index, you can rename the database table, and can be matched using matched_cols. 
#' @param index_cols Using the selected cols to match data with sqlite database. eg. c('chr', 'start'), 'rs'
#' @param full.matched_cols Using the selected cols to match data with selected partial data by index_cols limited.
#' @param inferior_col Inferior limit col, e.g. start
#' @param superior_col Superior limit col, e.g. end
#' @param return_col_index Setting the colnums need be returned
#' @param return_col_names Setting the returned colnum names
#' @param return_col_names_profix Setting the returned colnum names profix
#' @param format_dat_fun A function to process input data. eg. as.numeric(dat$start); as.character(dat$chr)
#' @param dbname_fixed Database path (txt, sqlite) or name (MySQL), default is NULL, and get from setdb_fun 
#' (Set value will fix the dbname, and will be added in sqlite.connenct.params and mysql.connect.params)
#' @param table_name_fixed Table name, default is NULL, and get from set_table_fun (Set value will fix the table.name)
#' (Set value will fix the table.name, and will be added in sqlite.connenct.params and mysql.connect.params)
#' @param setdb_fun A function to process the name, buildver, database.dir and get the database path (MySQL return NULL)
#' @param set_table_fun A function to process the name, buildver and get the final table name
#' @param format_db_tb_fun A function to process the selected database table that can be used to matched with your data
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
#' dbname_fixed = bed.sqlite, table_name_fixed = 'bed', 
#' db.type = 'sqlite', format_dat_fun = function(...) {
#' params = list(...);return(params[[1]])})
#' }
#' file.remove(bed.sqlite)
annotation.region.match <- function(dat = data.table(), anno.name = "", buildver = "hg19", 
  database.dir = Sys.getenv("annovarR_DB_DIR", ""), db_col_order = 1:3, index_cols = c("chr", 
    "start", "end"), full.matched_cols = "chr", inferior_col = "start", superior_col = "end", 
  return_col_index = 4, return_col_names = "", return_col_names_profix = "", format_dat_fun = format.cols, 
  dbname_fixed = NULL, table_name_fixed = NULL, setdb_fun = set.db, set_table_fun = set.table, 
  format_db_tb_fun = format.db.region.tb, db.type = "sqlite", db.file.prefix = NULL, 
  mysql.connect.params = list(), sqlite.connect.params = list(), fread.db.params = list(), 
  verbose = FALSE) {
  returned.list <- do.call(before.query.steps, list(dat = dat, anno.name = anno.name, 
    buildver = buildver, database.dir = database.dir, db_col_order = db_col_order, 
    index_cols = index_cols, matched_cols = c(full.matched_cols, inferior_col, 
      superior_col), format_dat_fun = format_dat_fun, dbname_fixed = dbname_fixed, 
    table_name_fixed = table_name_fixed, setdb_fun = setdb_fun, set_table_fun = set_table_fun, 
    db.type = db.type, db.file.prefix = db.file.prefix, mysql.connect.params = mysql.connect.params, 
    sqlite.connect.params = sqlite.connect.params, verbose = verbose))
  dat <- returned.list$dat
  database.con <- returned.list$database.con
  table.name <- returned.list$table.name
  tb.colnames <- returned.list$tb.colnames
  tb.colnames.raw <- returned.list$tb.colnames.raw
  index_cols.order <- returned.list$index_cols.order
  dat.names <- returned.list$dat.names
  params <- returned.list$params
  dbname <- returned.list$params
  rm(returned.list)
  gc()
  
  # Sync matched colsnames with reference database
  full.matched_cols.raw <- full.matched_cols
  inferior_col.raw <- inferior_col
  superior_col.raw <- superior_col
  for (i in c("full.matched_cols", "inferior_col", "superior_col")) {
    index <- match(colnames(dat), eval(parse(text = i)))
    text <- sprintf("%s <- '%s'", i, colnames(params)[!is.na(index)])
    eval(parse(text = text))
  }
  
  # Select data from database
  select.params <- list(db = database.con, table.name = table.name, full.matched_cols = full.matched_cols, 
    inferior_col = inferior_col, superior_col = superior_col, params = params, 
    db.type = db.type, fread.db.params = fread.db.params, verbose = verbose)
  selected.db.tb <- do.call(select.dat.region.match, select.params)
  matched_cols = c(full.matched_cols.raw, inferior_col.raw, superior_col.raw)
  return(do.call(after.query.steps, list(dat = dat, selected.db.tb = selected.db.tb, 
    format_db_tb_fun = format_db_tb_fun, return_col_index = return_col_index, 
    full.matched_cols = full.matched_cols, full.matched_cols.raw = full.matched_cols.raw, 
    inferior_col = inferior_col, inferior_col.raw = inferior_col.raw, superior_col = superior_col, 
    superior_col.raw = superior_col.raw, db_col_order = db_col_order, params = params, 
    return_col_names = return_col_names, return_col_names_profix = return_col_names_profix, 
    tb.colnames = tb.colnames, tb.colnames.raw = tb.colnames.raw, database.con = database.con, 
    db.type = db.type, dat.names = dat.names, get.final.table.fun = get.region.match.final.table, 
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
#' return_col_names = 'avSNP147', db.type = 'txt')
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
  needcols <- get.annotation.needcols(anno.name = anno.name, database.cfg = database.cfg)
  if (dim(dat) > 1 && length(colnames(dat)) > 0) {
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
  other_names <- anno.names[!anno.names %in% perl_annovar_names]
  result.list.1 <- lapply(other_names, function(x) {
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
