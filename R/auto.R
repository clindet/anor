#' Build annovarR database in sqlite (auto from extdata/config/database.toml)
#'
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases
#' @param overwrite Logical indicating wheather overwrite sqlite database, default is FALSE
#' @param append Logical indicating wheather append the data to sqlite database, 
#' default is FALSE
#' @param index Index name in sqlite 
#' @param db.type Setting the database type (sqlite, txt or mysql)
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param extra_fread_params Pass to \code{\link[ngstk]{batch_file}}, 
#' default is to get value from database.cfg
#' @param sqlite.build.params Extra params pass to \code{\link{sqlite.build}}
#' @param batch_lines Parameters pass to \code{\link[ngstk]{batch_file}}, 
#' default is 10000000
#' @param start_index default is 1, control the skip rows, n = (i-1) * batch_lines
#' @param new.colnames Use the fread determined colnames or use new colnames
#' @param verbose Logical indicating wheather print the extra log infomation
#' @export
#' @examples
#' i <- 'hg19_avsnp147'
#' database <- system.file('extdata', sprintf('demo/%s.txt', i), package = 'annovarR')
#' file.copy(database, sprintf('%s/%s.txt', tempdir(), i))
#' sqlite.auto.build('avsnp147', 'hg19', database.dir = tempdir(), verbose = TRUE)
#' unlink(sprintf('%s/%s.txt', tempdir(), i))
#' unlink(sprintf('%s/%s.sqlite', tempdir(), i))
sqlite.auto.build <- function(anno.name = "", buildver = "hg19", database.dir = "/path/", 
  overwrite = FALSE, append = FALSE, index = "chr_start_index", db.type = "sqlite", 
  database.cfg = system.file("extdata", "config/databases.toml", package = "annovarR"), 
  extra_fread_params = list(sep = "\t", header = TRUE, return_1L = FALSE), sqlite.build.params = list(fread.params = list(sep = "\t")), 
  batch_lines = 1e+07, start_index = 1, new.colnames = NULL, verbose = TRUE) {
  info.msg(sprintf("Auto build database %s %s in %s", buildver, anno.name, database.dir), 
    verbose = verbose)
  auto.parameters <- c("need.cols", "db.col.order", "setdb.fun", "set.table.fun", 
    "index.cols", "header", "dbname.fixed", "table.name.fixed")
  default.pars <- list()
  for (item in auto.parameters) {
    default.pars[[item]] <- get.cfg.value.by.name(anno.name, database.cfg, key = item, 
      coincident = TRUE, extra.list = list(anno.name = anno.name), rcmd.parse = TRUE)
  }
  dbname.fixed <- unlist(default.pars["dbname.fixed"])
  table.name.fixed <- unlist(default.pars["table.name.fixed"])
  if (is.null(dbname.fixed)) {
    filename <- do.call(default.pars[["setdb.fun"]], list(anno.name = anno.name, 
      buildver = buildver, database.dir = database.dir, db.type = "txt", db.file.prefix = "txt"))
    filename <- normalizePath(filename)
  } else {
    filename <- dbname.fixed
  }
  dbname <- str_replace(filename, "txt$", "sqlite")
  if (is.null(table.name.fixed)) {
    table.name <- do.call(default.pars[["set.table.fun"]], list(anno.name = anno.name, 
      buildver = buildver))
  } else {
    table.name <- table.name.fixed
  }
  sqlite.connect.params <- list(dbname = dbname, table.name = table.name)
  if (is.logical(default.pars[["header"]])) {
    extra_fread_params$header <- default.pars[["header"]]
  }
  
  build_fun <- function(x = "", i = 1, ...) {
    params <- list(...)
    new.colnames <- params$new.colnames
    if (all(new.colnames == x[1, ])) {
      colnames(x) <- new.colnames
      x <- x[-1, ]
    }
    if (i == 1) {
      sqlite.build.params <- config.list.merge(sqlite.build.params, list(dat = x, 
        sqlite.connect.params = sqlite.connect.params, verbose = verbose, 
        overwrite = overwrite, append = append, new.colnames = new.colnames))
      do.call(sqlite.build, sqlite.build.params)
    } else {
      sqlite.build.params <- config.list.merge(sqlite.build.params, list(dat = x, 
        sqlite.connect.params = sqlite.connect.params, verbose = verbose, 
        overwrite = FALSE, append = TRUE, new.colnames = new.colnames))
      do.call(sqlite.build, sqlite.build.params)
    }
  }
  if (is.null(new.colnames)) {
    new.colnames <- colnames(fread(filename, nrows = 1))
  }
  batch_file(filename = filename, batch_lines = batch_lines, handler = build_fun, 
    extra_params = list(new.colnames = new.colnames, sqlite.connect.params = sqlite.connect.params), 
    extra_fread_params = extra_fread_params, start_index = start_index)
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
#' @param database.dir Dir of the databases
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
sqlite.auto.index <- function(anno.name = "", buildver = "hg19", database.dir = "/path/", 
  index = "chr_start_index", db.type = "sqlite", database.cfg = system.file("extdata", 
    "config/databases.toml", package = "annovarR"), verbose = TRUE) {
  info.msg(sprintf("Auto build database %s %s in %s", buildver, anno.name, database.dir), 
    verbose = verbose)
  auto.parameters <- c("need.cols", "db.col.order", "setdb.fun", "set.table.fun", 
    "index.cols", "dbname.fixed", "table.name.fixed")
  default.pars <- list()
  for (item in auto.parameters) {
    default.pars[[item]] <- get.cfg.value.by.name(anno.name, database.cfg, key = item, 
      coincident = TRUE, extra.list = list(anno.name = anno.name), rcmd.parse = TRUE)
  }
  dbname.fixed <- unlist(default.pars["dbname.fixed"])
  table.name.fixed <- unlist(default.pars["table.name.fixed"])
  if (is.null(dbname.fixed)) {
    filename <- do.call(default.pars[["setdb.fun"]], list(anno.name = anno.name, 
      buildver = buildver, database.dir = database.dir, db.type = "txt", db.file.prefix = "txt"))
    filename <- normalizePath(filename)
  } else {
    filename <- dbname.fixed
  }
  dbname <- str_replace(filename, "txt$", "sqlite")
  if (is.null(table.name.fixed)) {
    table.name <- do.call(default.pars[["set.table.fun"]], list(anno.name = anno.name, 
      buildver = buildver))
  } else {
    table.name <- table.name.fixed
  }
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

#' Build annovarR database in mysql (auto from extdata/config/database.toml)
#'
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases
#' @param mysql.connect.params Mysql parameters, [host, dbname, table.name, user, password etc.]
#' @param overwrite Logical indicating wheather overwrite sqlite database, default is FALSE
#' @param append Logical indicating wheather append the data to sqlite database, 
#' default is FALSE
#' @param index Index name in sqlite 
#' @param db.type Setting the database type (sqlite, txt or mysql)
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param extra_fread_params Pass to \code{\link[ngstk]{batch_file}}, 
#' default is to get value from database.cfg
#' @param mysql.build.params Extra params pass to \code{\link{mysql.build}}
#' @param batch_lines Parameters pass to \code{\link[ngstk]{batch_file}}, 
#' default is 10000000
#' @param start_index default is 1, control the skip rows, n = (i-1) * batch_lines
#' @param new.colnames Use the fread determined colnames or use new colnames
#' @param verbose Logical indicating wheather print the extra log infomation
#' @export
#' @examples
#' i <- 'hg19_avsnp147'
#' database <- system.file('extdata', sprintf('demo/%s.txt', i), package = 'annovarR')
#' file.copy(database, sprintf('%s/%s.txt', tempdir(), i))
#' \dontrun{
#'  mysql.auto.build(anno.name = 'avsnp147', database.dir = tempdir(), 
#'  mysql.connect.params = list(user = 'username', password = 'password', 
#'  host = 'localhost', port = 3306, dbname = 'annovarR'))
#' }
mysql.auto.build <- function(anno.name = "", buildver = "hg19", database.dir = "/path/", 
  mysql.connect.params = list(user = "", password = "", host = "localhost", port = "3306"), 
  overwrite = FALSE, append = FALSE, index = "chr_start_index", db.type = "mysql", 
  database.cfg = system.file("extdata", "config/databases.toml", package = "annovarR"), 
  extra_fread_params = list(sep = "\t", header = TRUE, return_1L = FALSE), mysql.build.params = list(fread.params = list(sep = "\t")), 
  batch_lines = 1e+07, start_index = 1, new.colnames = NULL, verbose = TRUE) {
  info.msg(sprintf("Auto build database %s %s in mysql database [host:%s, port:%s]", 
    buildver, anno.name, mysql.connect.params$host, mysql.connect.params$port), 
    verbose = verbose)
  auto.parameters <- c("need.cols", "db.col.order", "setdb.fun", "set.table.fun", 
    "index.cols", "header", "dbname.fixed", "table.name.fixed")
  default.pars <- list()
  for (item in auto.parameters) {
    default.pars[[item]] <- get.cfg.value.by.name(anno.name, database.cfg, key = item, 
      coincident = TRUE, extra.list = list(anno.name = anno.name), rcmd.parse = TRUE)
  }
  dbname.fixed <- unlist(default.pars["dbname.fixed"])
  table.name.fixed <- unlist(default.pars["table.name.fixed"])
  if (is.null(dbname.fixed)) {
    filename <- do.call(default.pars[["setdb.fun"]], list(anno.name = anno.name, 
      buildver = buildver, database.dir = database.dir, db.type = "txt", db.file.prefix = "txt"))
    filename <- normalizePath(filename)
  } else {
    filename <- dbname.fixed
  }
  if (is.null(table.name.fixed)) {
    table.name <- do.call(default.pars[["set.table.fun"]], list(anno.name = anno.name, 
      buildver = buildver))
  } else {
    table.name <- table.name.fixed
  }
  dbname <- do.call(default.pars[["setdb.fun"]], list(anno.name = anno.name, buildver = buildver, 
    db.type = db.type, mysql.connect.params = mysql.connect.params))
  table.name <- do.call(default.pars[["set.table.fun"]], list(anno.name = anno.name, 
    buildver = buildver))
  mysql.connect.params <- config.list.merge(mysql.connect.params, list(dbname = dbname, 
    table.name = table.name))
  if (is.logical(default.pars[["header"]])) {
    extra_fread_params$header <- default.pars[["header"]]
  }
  
  build_fun <- function(x = "", i = 1, ...) {
    params <- list(...)
    new.colnames <- params$new.colnames
    if (all(new.colnames == x[1, ])) {
      colnames(x) <- new.colnames
      x <- x[-1, ]
    }
    if (i == 1) {
      mysql.build.params <- config.list.merge(mysql.build.params, list(dat = x, 
        mysql.connect.params = mysql.connect.params, verbose = verbose, overwrite = overwrite, 
        append = append, new.colnames = new.colnames))
      do.call(mysql.build, mysql.build.params)
    } else {
      mysql.build.params <- config.list.merge(mysql.build.params, list(dat = x, 
        mysql.connect.params = mysql.connect.params, verbose = verbose, overwrite = FALSE, 
        append = TRUE, new.colnames = new.colnames))
      do.call(mysql.build, mysql.build.params)
    }
  }
  if (is.null(new.colnames)) {
    new.colnames <- colnames(fread(filename, nrows = 1))
  }
  batch_file(filename = filename, batch_lines = batch_lines, handler = build_fun, 
    extra_params = list(new.colnames = new.colnames, mysql.connect.params = mysql.connect.params), 
    extra_fread_params = extra_fread_params, start_index = start_index)
  db.colnames <- mysql.tb.colnames(mysql.connect.params)
  db.colnames <- db.colnames[default.pars[["db.col.order"]]]
  order <- match(default.pars[["index.cols"]], default.pars[["need.cols"]])
  cols <- db.colnames[order]
  mysql.index(mysql.connect.params = mysql.connect.params, cols = cols, index = index, 
    verbose = verbose)
  indexes <- mysql.tb.indexes(mysql.connect.params = mysql.connect.params)
  if (index %in% indexes$name) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Index annovarR database in mysql (auto from extdata/config/database.toml)
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases (mysql no need)
#' @param mysql.connect.params Mysql parameters, [host, dbname, table.name, user, password etc.]
#' @param index Index name in sqlite 
#' @param db.type Setting the database type (sqlite, txt or mysql)
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param verbose Logical indicating wheather print the extra log infomation
#' @export
#' @examples
#' i <- 'hg19_avsnp147'
#' database <- system.file('extdata', sprintf('demo/%s.txt', i), package = 'annovarR')
#' file.copy(database, sprintf('%s/%s.txt', tempdir(), i))
#' \dontrun{
#'  mysql.auto.index(anno.name = 'avsnp147', database.dir = tempdir(), 
#'  mysql.connect.params = list(user = 'username', password = 'password', 
#'  host = 'localhost', port = 3306, dbname = 'annovarR'))
#' }
mysql.auto.index <- function(anno.name = "", buildver = "hg19", database.dir = "/path/", 
  mysql.connect.params = list(user = "", password = "", host = "localhost", port = "3306"), 
  index = "chr_start_index", db.type = "mysql", database.cfg = system.file("extdata", 
    "config/databases.toml", package = "annovarR"), verbose = TRUE) {
  info.msg(sprintf("Auto index database %s %s in mysql database [host:%s, port:%s]", 
    buildver, anno.name, mysql.connect.params$host, mysql.connect.params$port), 
    verbose = verbose)
  auto.parameters <- c("need.cols", "db.col.order", "setdb.fun", "set.table.fun", 
    "index.cols", "dbname.fixed", "table.name.fixed")
  default.pars <- list()
  for (item in auto.parameters) {
    default.pars[[item]] <- get.cfg.value.by.name(anno.name, database.cfg, key = item, 
      coincident = TRUE, extra.list = list(anno.name = anno.name), rcmd.parse = TRUE)
  }
  dbname.fixed <- unlist(default.pars["dbname.fixed"])
  table.name.fixed <- unlist(default.pars["table.name.fixed"])
  if (is.null(dbname.fixed)) {
    filename <- do.call(default.pars[["setdb.fun"]], list(anno.name = anno.name, 
      buildver = buildver, database.dir = database.dir, db.type = "txt", db.file.prefix = "txt"))
    filename <- normalizePath(filename)
  } else {
    filename <- dbname.fixed
  }
  if (is.null(table.name.fixed)) {
    table.name <- do.call(default.pars[["set.table.fun"]], list(anno.name = anno.name, 
      buildver = buildver))
  } else {
    table.name <- table.name.fixed
  }
  dbname <- do.call(default.pars[["setdb.fun"]], list(anno.name = anno.name, buildver = buildver, 
    db.type = db.type, mysql.connect.params = mysql.connect.params))
  mysql.connect.params <- config.list.merge(mysql.connect.params, list(dbname = dbname, 
    table.name = table.name))
  db.colnames <- mysql.tb.colnames(mysql.connect.params)
  db.colnames <- db.colnames[default.pars[["db.col.order"]]]
  order <- match(default.pars[["index.cols"]], default.pars[["need.cols"]])
  cols <- db.colnames[order]
  status <- mysql.index(mysql.connect.params = mysql.connect.params, cols = cols, 
    index = index, verbose = verbose)
  indexes <- mysql.tb.indexes(mysql.connect.params = mysql.connect.params)
  if (index %in% indexes$name) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Auto to annotation accodring the database.cfg annovarR supported anno.names
annotation.auto <- function(dat = NULL, anno.name = NULL, return.col.names = NULL, 
  return.col.names.profix = NULL, return.col.index = NULL, db.col.order = NULL, 
  index.cols = NULL, matched.cols = NULL, full.matched.cols = NULL, inferior.col = NULL, 
  superior.col = NULL, dbname.fixed = NULL, table.name.fixed = NULL, setdb.fun = NULL, 
  set.table.fun = NULL, format.db.tb.fun = NULL, format.dat.fun = NULL, db.file.prefix = NULL, 
  is.region = NULL, database.cfg = NULL, ...) {
  
  used.names.1 <- formalArgs(annotation.cols.match)
  used.names.2 <- formalArgs(annotation.region.match)
  used.names <- unique(c(used.names.1, used.names.2))
  params <- list(...)
  params[!names(params) %in% used.names] <- NULL
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
    "superior.col", "is.region", "dbname.fixed", "table.name.fixed", "return.col.names.profix")
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
  params$is.region <- NULL
  if (is.null(is.region) || !is.region) {
    do.call("annotation.cols.match", config.list.merge(params, list(dat = dat, 
      anno.name = anno.name, setdb.fun = eval.parse.null(params[["setdb.fun"]]), 
      set.table.fun = eval.parse.null(params[["set.table.fun"]]), format.db.tb.fun = eval.parse.null(params[["format.db.tb.fun"]]), 
      format.dat.fun = eval.parse.null(params[["format.dat.fun"]]))))
  } else {
    do.call("annotation.region.match", config.list.merge(params, list(dat = dat, 
      anno.name = anno.name, setdb.fun = eval.parse.null(params[["setdb.fun"]]), 
      set.table.fun = eval.parse.null(params[["set.table.fun"]]), format.db.tb.fun = eval.parse.null(params[["format.db.tb.fun"]]), 
      format.dat.fun = eval.parse.null(params[["format.dat.fun"]]))))
  }
}

# A auto recognition function to get the annotation function from database.cfg
get.annotation.func <- function(anno.name = "", database.cfg = system.file("extdata", 
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

eval.parse.null <- function(text = "") {
  if (is.null(text)) {
    return(NULL)
  } else {
    x <- eval(parse(text = text))
  }
  return(x)
}

# Function to annotate variants use ANNOVAR
annovar.auto <- function(anno.name = NULL, cmd.used = NULL, database.cfg = NULL, 
  annovar.anno.names = "", ...) {
  params <- list(...)
  used.names <- formalArgs(annovar)
  auto.parameters <- c("cmd.used")
  for (item in auto.parameters) {
    item.value <- eval(parse(text = item))
    if (is.null(item.value)) {
      params[[item]] <- get.cfg.value.by.name(anno.name, database.cfg, key = item, 
        coincident = TRUE, extra.list = list(anno.name = anno.name), rcmd.parse = TRUE)
    } else {
      params[[item]] <- item.value
    }
  }
  if (!is.null(params$dat) && is.data.table(params$dat) && nrow(params$dat) > 0) {
    tmpfn <- tempfile()
    fwrite(params$dat, tmpfn, sep = "\t", row.names = FALSE, quote = FALSE, col.names = FALSE)
    params$dat <- NULL
    params$input.file <- tmpfn
  }
  params[!names(params) %in% used.names] <- NULL
  anno.name <- str_replace(anno.name, "perl_annovar_", "")
  if (anno.name == "merge") {
    params <- config.list.merge(params, list(anno.names = annovar.anno.names))
  } else {
    params <- config.list.merge(params, list(anno.names = anno.name))
  }
  if (!is.null(params$extra.params) && !params$vcfinput && str_detect(params$extra.params, 
    "--csvout")) {
    out_prefix <- "csv"
    sep = ","
  } else {
    out_prefix <- "txt"
    sep = "\t"
  }
  if (is.null(params$out) || params$out == "") {
    outfn <- sprintf("%s.%s_multianno.%s", params$input.file, params$buildver, 
      out_prefix)
  } else {
    outfn <- sprintf("%s.%s_multianno.%s", params$out, params$buildver, out_prefix)
    outfn.dirname <- dirname(outfn)
    if (!dir.exists(outfn.dirname)) {
      dir.create(outfn.dirname, recursive = TRUE)
    }
  }
  cmd <- do.call(annovar, params)
  if (file.exists(outfn)) {
    fn <- file(outfn, "r")
    header <- readLines(outfn, n = 1)
    header <- str_split(header, sep)[[1]]
    close(fn)
    outdat <- fread(outfn, sep = sep, header = FALSE, skip = 1)
    colnames(outdat)[1:length(header)] <- header
    if (all(outdat[1, 1:length(header)] == header)) {
      outdat <- outdat[-1, ]
    }
    attr(outdat, "annovar_out_file") <- outfn
    attr(outdat, "cmd") <- cmd
    return(outdat)
  } else if ("debug" %in% names(params) && !params$debug) {
    return(FALSE)
  }
}

bioc.auto <- function(anno.name = NULL, database.cfg = NULL, bioc_dbname = NULL, 
  keytype = NULL, columns = NULL, ...) {
  dependence_db <- NULL
  params <- list(...)
  auto.parameters <- c("dependence_db", "keytype", "columns")
  for (item in auto.parameters) {
    item.value <- eval(parse(text = item))
    if (is.null(item.value)) {
      params[[item]] <- get.cfg.value.by.name(anno.name, database.cfg, key = item, 
        coincident = TRUE, extra.list = list(anno.name = anno.name), rcmd.parse = TRUE)
    } else {
      params[[item]] <- item.value
    }
  }
  if (is.null(bioc_dbname)) {
    bioc_dbname_raw <- params[["dependence_db"]]
    bioc_dbname <- str_replace(bioc_dbname_raw, "^db_bioc_", "")
  } else if (!str_detect(bioc_dbname, "^db_bioc_")) {
    bioc_dbname_raw <- paste0("db_bioc_", bioc_dbname)
  } else {
    bioc_dbname_raw <- bioc_dbname
  }
  pkg.raw <- (.packages())
  download.database(bioc_dbname_raw)
  tryCatch(suppressMessages(do.call("require", list(package = bioc_dbname))))
  pkg.new <- (.packages())
  params$x <- eval.parse.null(bioc_dbname)
  params$keys <- params$dat
  params$dat <- NULL
  x <- as.data.table(do.call("select", params))
  for (i in pkg.new[!pkg.new %in% pkg.raw]) {
    detach(paste0("package:", i), character.only = TRUE)
  }
  return(x)
}

vep.auto <- function(anno.name = NULL, database.cfg = NULL, ...) {
  params <- list(...)
  used.names <- formalArgs(vep)
  params[!names(params) %in% used.names] <- NULL
  cmd <- do.call("vep", params)
  out <- params$out
  if (file.exists(out)) {
    x <- read.vcfR(out)
    x <- as.data.table(cbind(getFIX(x), INFO2df(x)))
    attr(outdat, "vep_out_file") <- out
    attr(outdat, "cmd") <- cmd
    return(outdat)
  } else if (is.logical(params$debug) && !params$debug) {
    return(FALSE)
  }
}

vcfanno.auto <- function(anno.name = NULL, database.cfg = NULL, ...) {
  params <- list(...)
  vcfanno.database.cfg <- NULL
  base_path <- NULL
  lua <- NULL
  auto.parameters <- c("vcfanno.database.cfg", "base_path", "lua")
  for (item in auto.parameters) {
    item.value <- eval(parse(text = item))
    if (is.null(item.value)) {
      params[[item]] <- get.cfg.value.by.name(anno.name, database.cfg, key = item, 
        coincident = TRUE, extra.list = list(anno.name = anno.name), rcmd.parse = TRUE)
    } else {
      params[[item]] <- item.value
    }
  }
  params$input.file <- normalizePath(params$input.file, mustWork = FALSE)
  params$out <- normalizePath(params$out, mustWork = FALSE)
  params$vcfanno <- normalizePath(params$vcfanno, mustWork = FALSE)
  params$lua <- normalizePath(params$lua, mustWork = FALSE)
  out <- params$out
  if (!dir.exists(dirname(out))) {
    dir.create(dirname(out))
  }
  used.names <- formalArgs(vcfanno)
  params[!names(params) %in% used.names] <- NULL
  cmd <- do.call("vcfanno", params)
  if (file.exists(out)) {
    x <- read.vcfR(out)
    outdat <- as.data.table(cbind(getFIX(x), INFO2df(x)))
    attr(outdat, "vcfanno_out_file") <- out
    attr(outdat, "cmd") <- cmd
    return(outdat)
  } else if (is.logical(params$debug) && !params$debug) {
    return(FALSE)
  }
}
