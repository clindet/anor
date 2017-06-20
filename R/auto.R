#' Build annovarR database in sqlite (auto from extdata/config/database.toml)
#'
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases (mysql no need)
#' @param index Index name in sqlite 
#' @param db.type Setting the database type (sqlite, txt or mysql)
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param verbose Logical indicating wheather print the extra log infomation
#' @export
#' @examples
#' i <- 'hg19_avsnp147'
#' database <- system.file('extdata', sprintf('demo/%s.txt', i), package = 'annovarR')
#' file.copy(database, sprintf('%s/%s.txt', tempdir(), i))
#' sqlite.auto.build('avsnp147', 'hg19', database.dir = tempdir(), verbose = TRUE)
#' unlink(sprintf('%s/%s.txt', tempdir(), i))
#' unlink(sprintf('%s/%s.sqlite', tempdir(), i))
sqlite.auto.build <- function(anno.name, buildver = "hg19", database.dir = "/path/", 
  index = "chr_start_index", db.type = "sqlite", database.cfg = system.file("extdata", 
    "config/databases.toml", package = "annovarR"), verbose = TRUE) {
  info.msg(sprintf("Auto build database %s %s in %s", buildver, anno.name, database.dir), 
    verbose = verbose)
  auto.parameters <- c("need.cols", "db.col.order", "setdb.fun", "set.table.fun", 
    "index.cols")
  default.pars <- list()
  for (item in auto.parameters) {
    default.pars[[item]] <- get.cfg.value.by.name(anno.name, database.cfg, key = item, 
      coincident = TRUE, extra.list = list(anno.name = anno.name), rcmd.parse = TRUE)
  }
  filename <- do.call(default.pars[["setdb.fun"]], list(anno.name = anno.name, 
    buildver = buildver, database.dir = database.dir, db.type = "txt", db.file.prefix = "txt"))
  dbname <- str_replace(filename, "txt$", "sqlite")
  table.name <- do.call(default.pars[["set.table.fun"]], list(anno.name = anno.name, 
    buildver = buildver))
  sqlite.connect.params <- list(dbname = dbname, table.name = table.name)
  sqlite.build(filename = filename, sqlite.connect.params = sqlite.connect.params, 
    verbose = verbose)
  db.colnames <- sqlite.tb.colnames(sqlite.connect.params)
  db.colnames <- db.colnames[default.pars[["db.col.order"]]]
  order <- match(default.pars[["index.cols"]], default.pars[["need.cols"]])
  cols <- db.colnames[order]
  sqlite.index(sqlite.connect.params = sqlite.connect.params, cols = cols, index = index, 
    verbose = verbose)
  indexes <- sqlite.tb.indexes(sqlite.connect.params = sqlite.connect.params)
  if (index %in% indexes$name) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Index annovarR database in sqlite (auto from extdata/config/database.toml)
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases (mysql no need)
#' @param index Index name in sqlite 
#' @param db.type Setting the database type (sqlite, txt or mysql)
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param verbose Logical indicating wheather print the extra log infomation
#' @export
#' @examples
#' i <- 'hg19_avsnp147'
#' database <- system.file('extdata', sprintf('demo/%s.txt', i), package = 'annovarR')
#' file.copy(database, sprintf('%s/%s.txt', tempdir(), i))
#' sqlite.auto.build('avsnp147', 'hg19', database.dir = tempdir(), verbose = TRUE)
#' sqlite.auto.index('avsnp147', 'hg19', database.dir = tempdir(), index = 'chr_start_index2',
#' verbose = TRUE)
#' unlink(sprintf('%s/%s.txt', tempdir(), i))
#' unlink(sprintf('%s/%s.sqlite', tempdir(), i))
sqlite.auto.index <- function(anno.name, buildver = "hg19", database.dir = "/path/", 
  index = "chr_start_index", db.type = "sqlite", database.cfg = system.file("extdata", 
    "config/databases.toml", package = "annovarR"), verbose = TRUE) {
  info.msg(sprintf("Auto build database %s %s in %s", buildver, anno.name, database.dir), 
    verbose = verbose)
  auto.parameters <- c("need.cols", "db.col.order", "setdb.fun", "set.table.fun", 
    "index.cols")
  default.pars <- list()
  for (item in auto.parameters) {
    default.pars[[item]] <- get.cfg.value.by.name(anno.name, database.cfg, key = item, 
      coincident = TRUE, extra.list = list(anno.name = anno.name), rcmd.parse = TRUE)
  }
  filename <- do.call(default.pars[["setdb.fun"]], list(anno.name = anno.name, 
    buildver = buildver, database.dir = database.dir, db.type = "txt"))
  dbname <- str_replace(filename, "txt$", "sqlite")
  table.name <- do.call(default.pars[["set.table.fun"]], list(anno.name = anno.name, 
    buildver = buildver))
  sqlite.connect.params <- list(dbname = dbname, table.name = table.name)
  db.colnames <- sqlite.tb.colnames(sqlite.connect.params)
  db.colnames <- db.colnames[default.pars[["db.col.order"]]]
  order <- match(default.pars[["index.cols"]], default.pars[["need.cols"]])
  cols <- db.colnames[order]
  status <- sqlite.index(sqlite.connect.params = sqlite.connect.params, cols = cols, 
    index = index, verbose = verbose)
  indexes <- sqlite.tb.indexes(sqlite.connect.params = sqlite.connect.params)
  if (index %in% indexes$name) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Auto to annotation accodring the database.cfg
annotation.auto <- function(dat, anno.name, return.col.names = NULL, return.col.index = NULL, 
  db.col.order = NULL, index.cols = NULL, matched.cols = NULL, full.matched.cols = NULL, 
  inferior.col = NULL, superior.col = NULL, dbname.fixed = NULL, table.name.fixed = NULL, 
  setdb.fun = NULL, set.table.fun = NULL, format.db.tb.fun = NULL, format.dat.fun = NULL, 
  db.file.prefix = NULL, database.cfg = system.file("extdata", "config/databases.toml", 
    package = "annovarR"), is.region = NULL, ...) {
  
  # dat.need.names <- get.cfg.value.by.name(anno.name, database.cfg, key =
  # 'need.cols', coincident = TRUE, extra.list = list(anno.name = anno.name),
  # rcmd.parse = TRUE)
  
  # dat <- dat[, colnames(dat) %in% dat.need.names, with = FALSE]
  
  supported.auto.names <- get.annotation.names(database.cfg = database.cfg)
  if (!anno.name %in% supported.auto.names) {
    stop(sprintf("%s not be supprted by annotation.auto, please check the name and %s.", 
      anno.name, database.cfg))
  }
  
  auto.parameters <- c("return.col.names", "return.col.index", "db.col.order", 
    "index.cols", "matched.cols", "setdb.fun", "set.table.fun", "format.db.tb.fun", 
    "format.dat.fun", "db.file.prefix", "full.matched.cols", "inferior.col", 
    "superior.col", "is.region")
  params <- list()
  for (item in auto.parameters) {
    item.value <- eval(parse(text = item))
    if (is.null(item.value)) {
      params[[item]] <- get.cfg.value.by.name(anno.name, database.cfg, key = item, 
        coincident = TRUE, extra.list = list(anno.name = anno.name), rcmd.parse = TRUE)
    } else {
      params[[item]] <- item.value
    }
  }
  is.region <- params[["is.region"]]
  if (is.null(is.region) || !is.region) {
    annotation.cols.match(dat = dat, anno.name = anno.name, return.col.names = params[["return.col.names"]], 
      return.col.index = params[["return.col.index"]], db.col.order = params[["db.col.order"]], 
      index.cols = params[["index.cols"]], matched.cols = params[["matched.cols"]], 
      setdb.fun = eval(parse(text = params[["setdb.fun"]])), set.table.fun = eval(parse(text = params[["set.table.fun"]])), 
      format.db.tb.fun = eval(parse(text = params[["format.db.tb.fun"]])), 
      dbname.fixed = dbname.fixed, table.name.fixed = table.name.fixed, format.dat.fun = eval(parse(text = params[["format.dat.fun"]])), 
      db.file.prefix = params[["db.file.prefix"]], ...)
  } else {
    annotation.region.match(dat = dat, anno.name = anno.name, return.col.names = params[["return.col.names"]], 
      return.col.index = params[["return.col.index"]], db.col.order = params[["db.col.order"]], 
      index.cols = params[["index.cols"]], full.matched.cols = params[["full.matched.cols"]], 
      inferior.col = params[["inferior.col"]], superior.col = params[["superior.col"]], 
      setdb.fun = eval(parse(text = params[["setdb.fun"]])), set.table.fun = eval(parse(text = params[["set.table.fun"]])), 
      format.db.tb.fun = eval(parse(text = params[["format.db.tb.fun"]])), 
      dbname.fixed = dbname.fixed, table.name.fixed = table.name.fixed, format.dat.fun = eval(parse(text = params[["format.dat.fun"]])), 
      db.file.prefix = params[["db.file.prefix"]], ...)
  }
}

# A auto recognition function to get the annotation function from database.cfg
get.annotation.func <- function(anno.name, database.cfg = system.file("extdata", 
  "config/databases.toml", package = "annovarR")) {
  all.supported.db <- get.annotation.names(database.cfg)
  if (!(anno.name %in% all.supported.db)) {
    stop(sprintf("%s not be supported.", anno.name))
  }
  config <- configr::read.config(database.cfg)
  config <- config[names(config) != "Title"]
  index <- lapply(config, function(x) {
    anno.name %in% x[["versions"]]
  })
  index <- unlist(index)
  config <- config[[names(config)[index]]]
  return(config$func)
}
