#' A position annotation utils that can be used to write a yourself annotation function
#'
#' @param dat.list A list including all of your data, eg. list(chr=c(1,2,3), start=c(1111,1112,1113))
#' @param name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param builder Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases
#' @param db.col.order Using the index, you can rename the database table, and can be matched using matched.cols. 
#' @param index.col Using the selected cols to match data with sqlite database. eg. c('chr', 'start'), 'rs'
#' @param matched.cols Using the selected cols to match data with selected partial data by index.col limited.
#' @param return.col.index Setting the colnums need be returned
#' @param return.col.names Setting the returned colnum names
#' @param format.dat.fun A function to process input data. eg. as.numeric(dat.list$start); as.character(dat.list$chr)
#' @param setdb.fun A function to process the name, builder, database.dir and get the database path
#' @param set.table.fun A function to process the name, builder and get the final table name
#' @param format.db.tb.fun A function to process the selected database table that can be used to matched with your data
#' @param db.type Setting the database type (sqlite or txt)
#' @param verbose Logical indicating wheather print the extra log infomation
#' @export
#' @examples
#' chr <- c('chr1', 'chr2', 'chr1')
#' start <- c('10020', '10020', '10020')
#' end <- c('10020', '10020', '10020')
#' ref <- c('A', 'A', 'A')
#' alt <- c('-', '-', '-')
#' database <- system.file('extdata', 'demo/hg19_avsnp147.sqlite', package = 'annovarR')
#' database.dir <- dirname(database)
#' dat.list <- list(chr = chr, start = start, end = end, ref = ref, alt = alt)
#' x <- annotation.pos.utils(dat.list, 'avsnp147', database.dir = database.dir, 
#' return.col.names = 'avSNP147')
annotation.pos.utils <- function(dat.list = list(), name = "", builder = "hg19", 
  database.dir = Sys.getenv("annovarR_DB_DIR", ""), db.col.order = 1:5, index.col = c("chr", 
    "start"), matched.cols = c("chr", "start", "end", "ref", "alt"), return.col.index = 6, 
  return.col.names = "", format.dat.fun = format.cols, setdb.fun = set.db, set.table.fun = set.table, 
  format.db.tb.fun = format.db.tb, db.type = "sqlite", verbose = FALSE) {
  dat.list.names <- names(dat.list)
  if (database.dir == "") {
    info.msg("Parameter database.dir not be setted.", verbose = verbose)
  } else {
    if (!dir.exists(database.dir)) {
      info.msg(sprintf("%s directory not existed.", database.dir), verbose = verbose)
      return(FALSE)
    }
  }
  dat.list <- format.dat.fun(dat.list)
  db.path <- setdb.fun(name, builder, database.dir, db.type)
  db <- db.path
  table.name <- set.table.fun(name, builder)
  info.msg(sprintf("Setting up connection: %s sqlite databse.", db.path), verbose = verbose)
  db <- connect.db(db, db.type)
  tb.colnames <- db.tb.colnames(db.path, table.name, db.type)
  info.msg("Database colnames:%s", paste0(tb.colnames, collapse = ", "), verbose = verbose)
  params = dat.list[names(dat.list) %in% index.col]
  index.col.order <- match(names(dat.list), index.col)
  selected.db.tb <- select.dat(db, table.name, tb.colnames[index.col.order], params = params, 
    db.type = db.type)
  selected.db.tb <- format.db.tb.fun(selected.db.tb)
  if (nrow(selected.db.tb) == 0) {
    empty.col <- return.empty.col(dat.list, tb.colnames, return.col.index, return.col.names)
    return(empty.col)
  }
  selected.db.tb <- sync.colnames(selected.db.tb, db.col.order, dat.list.names)
  tb.colnames <- colnames(selected.db.tb)
  info.msg("After sync, the colnames is %s", paste0(tb.colnames, collapse = ", "), 
    verbose = verbose)
  print.vb(head(selected.db.tb), verbose = verbose)
  input.index <- get.input.index(dat.list, matched.cols)
  ref.index <- get.ref.index(selected.db.tb, matched.cols)
  info.msg(sprintf("Disconnect the connection with the %s sqlite databse.", db.path), 
    verbose = verbose)
  disconnect.db(db, db.type)
  index <- match(input.index, ref.index)
  selected.db.tb <- selected.db.tb[index, return.col.index]
  result <- data.frame(selected.db.tb)
  if (return.col.names != "") {
    colnames(result) <- return.col.names
  } else {
    colnames(result) <- tb.colnames[return.col.index]
  }
  return(result)
}

#' Annotation function
#'
#' @param dat.list A list including all of your data, eg. list(chr=c(1,2,3), start=c(1111,1112,1113))
#' @param name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all .etc.
#' @param builder Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases
#' @param db.type Setting the database type (sqlite or txt)
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param func Function to anntate the dat.list data, default is to search the function in extdata/database.toml
#' @param ... Other parametes see \code{\link{annotation.pos.utils}}
#' @export
#' @examples
#' chr <- c('chr1', 'chr2', 'chr1')
#' start <- c('10020', '10020', '10020')
#' end <- c('10020', '10020', '10020')
#' ref <- c('A', 'A', 'A')
#' alt <- c('-', '-', '-')
#' database <- system.file('extdata', 'demo/hg19_avsnp147.sqlite', package = 'annovarR')
#' database.dir <- dirname(database)
#' dat.list <- list(chr = chr, start = start, end = end, ref = ref, alt = alt)
#' x <- annotation(dat.list, 'avsnp147', database.dir = database.dir, return.col.names = 'avSNP147')
annotation <- function(dat.list = list(), name = "", builder = "hg19", database.dir = Sys.getenv("annovarR_DB_DIR", 
  ""), db.type = NULL, database.cfg = system.file("extdata", "config/databases.toml", 
  package = "annovarR"), func = NULL, ...) {
  result <- NULL
  if (is.null(db.type)) {
    db.type <- get.annotation.dbtype(name, database.cfg = database.cfg)
  }
  if (is.null(func)) {
    func <- get.annotation.func(name, database.cfg = database.cfg)
    func <- eval(parse(text = func))
  }
  text <- "result <- func(dat.list = dat.list, name = name, builder = builder, database.dir = database.dir, db.type = db.type, ...)"
  eval(parse(text = text))
  return(result)
}

annotation.auto <- function(dat.list, name, return.col.names = NULL, return.col.index = NULL, 
  db.col.order = NULL, matched.cols = NULL, setdb.fun = NULL, set.table.fun = NULL, 
  format.db.tb.fun = NULL, database.cfg = system.file("extdata", "config/databases.toml", 
    package = "annovarR"), ...) {
  
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
  annotation.pos.utils(dat.list = dat.list, name = name, return.col.names = para.values[["return.col.names"]], 
    return.col.index = para.values[["return.col.index"]], db.col.order = para.values[["db.col.order"]], 
    matched.cols = para.values[["matched.cols"]], setdb.fun = eval(parse(text = para.values[["setdb.fun"]])), 
    set.table.fun = eval(parse(text = para.values[["set.table.fun"]])), format.db.tb.fun = eval(parse(text = para.values[["format.db.tb.fun"]])), 
    ...)
}
