#' Build annovarR database in sqlite (auto from extdata/config/database.toml)
#'
#' @param name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases (mysql no need)
#' @param index Index name in sqlite 
#' @param db.type Setting the database type (sqlite, txt or mysql)
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param verbose Logical indicating wheather print the extra log infomation
#' @export
#' @examples
#' i <- "hg19_avsnp147"
#' database <- system.file("extdata", sprintf("demo/%s.txt", i), package = "annovarR")
#' file.copy(database, sprintf("%s/%s.txt", tempdir(), i))
#' sqlite.auto.build("avsnp147", "hg19", database.dir = tempdir(), verbose = TRUE)
#' unlink(sprintf("%s/%s.txt", tempdir(), i))
#' unlink(sprintf("%s/%s.sqlite", tempdir(), i))
sqlite.auto.build <- function(name, buildver = "hg19", database.dir = "/path/", index="chr_start_index", db.type = "sqlite", 
                       database.cfg = system.file("extdata", "config/databases.toml", package = "annovarR"), 
                       verbose = TRUE) {
  info.msg(sprintf("Auto build database %s %s in %s", buildver, name, database.dir), verbose = verbose)
  auto.parameters <- c("need.cols", "db.col.order", 
    "setdb.fun", "set.table.fun", "index.col")
  default.pars <- list()
  for (item in auto.parameters) {
      default.pars[[item]] <- get.cfg.value.by.name(name, database.cfg, key = item, 
        coincident = TRUE, extra.list = list(name = name), rcmd.parse = TRUE)
  }
  filename <- do.call(default.pars[["setdb.fun"]], list(name = name, buildver = buildver, 
                                                       database.dir = database.dir, db.type = "txt")) 
  sqlite.path <- str_replace(filename, "txt$", "sqlite")
  table.name <- do.call(default.pars[["set.table.fun"]], list(name = name, buildver = buildver))
  sqlite.connect.params <- list(sqlite.path = sqlite.path, table.name = table.name)
  sqlite.build(filename = filename, sqlite.connect.params = sqlite.connect.params,
               verbose = verbose)
  db.colnames <- sqlite.tb.colnames(sqlite.connect.params)
  db.colnames <- db.colnames[default.pars[["db.col.order"]]]
  order <- match(default.pars[["index.col"]], default.pars[["need.cols"]])
  cols <- db.colnames[order]
  sqlite.index(sqlite.connect.params = sqlite.connect.params, cols = cols, index = index)
  indexes <- sqlite.tb.indexes(sqlite.connect.params = sqlite.connect.params)
  if (index %in% indexes$name) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Index annovarR database in sqlite (auto from extdata/config/database.toml)
#' @param name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param buildver Genome version, hg19, hg38, mm10 and others
#' @param database.dir Dir of the databases (mysql no need)
#' @param index Index name in sqlite 
#' @param db.type Setting the database type (sqlite, txt or mysql)
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param verbose Logical indicating wheather print the extra log infomation
#' @export
#' @examples
#' i <- "hg19_avsnp147"
#' database <- system.file("extdata", sprintf("demo/%s.txt", i), package = "annovarR")
#' file.copy(database, sprintf("%s/%s.txt", tempdir(), i))
#' sqlite.auto.build("avsnp147", "hg19", database.dir = tempdir(), verbose = TRUE)
#' sqlite.auto.index("avsnp147", "hg19", database.dir = tempdir(), index = "chr_start_index2",
#' verbose = TRUE)
#' unlink(sprintf("%s/%s.txt", tempdir(), i))
#' unlink(sprintf("%s/%s.sqlite", tempdir(), i))
sqlite.auto.index <- function(name, buildver = "hg19", database.dir = "/path/", index="chr_start_index", db.type = "sqlite", 
                       database.cfg = system.file("extdata", "config/databases.toml", package = "annovarR"), 
                       verbose = TRUE) {
  info.msg(sprintf("Auto build database %s %s in %s", buildver, name, database.dir), verbose = verbose)
  auto.parameters <- c("need.cols", "db.col.order", 
    "setdb.fun", "set.table.fun", "index.col")
  default.pars <- list()
  for (item in auto.parameters) {
      default.pars[[item]] <- get.cfg.value.by.name(name, database.cfg, key = item, 
        coincident = TRUE, extra.list = list(name = name), rcmd.parse = TRUE)
  }
  filename <- do.call(default.pars[["setdb.fun"]], list(name = name, buildver = buildver, 
                                                       database.dir = database.dir, db.type = "txt")) 
  sqlite.path <- str_replace(filename, "txt$", "sqlite")
  table.name <- do.call(default.pars[["set.table.fun"]], list(name = name, buildver = buildver))
  sqlite.connect.params <- list(sqlite.path = sqlite.path, table.name = table.name)
  db.colnames <- sqlite.tb.colnames(sqlite.connect.params)
  db.colnames <- db.colnames[default.pars[["db.col.order"]]]
  order <- match(default.pars[["index.col"]], default.pars[["need.cols"]])
  cols <- db.colnames[order]
  status <- sqlite.index(sqlite.connect.params = sqlite.connect.params, cols = cols, index = index)
  indexes <- sqlite.tb.indexes(sqlite.connect.params = sqlite.connect.params)
  if (index %in% indexes$name) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
