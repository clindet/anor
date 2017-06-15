# Sync database and input table colnames
sync.colnames <- function(result, col.order, col.names) {
  colnames(result)[col.order] <- col.names
  return(result)
}

# Return NA cols using ordered colnames
return.empty.col <- function(dat.list, tb.colnames, return.col.index, return.col.names) {
  result <- NULL
  for (i in 1:length(return.col.index)) {
    result <- cbind(result, rep(NA, length(dat.list[[1]])))
  }
  result <- as.data.table(result)
  if (any(return.col.names != "")) {
    colnames(result) <- return.col.names
  } else {
    colnames(result) <- tb.colnames[return.col.index]
  }
  return(result)
}

# A auto recognition function to get the annotation function from database.cfg
get.annotation.func <- function(name, database.cfg = system.file("extdata", "config/databases.toml", 
  package = "annovarR")) {
  all.supported.db <- get.annotation.names(database.cfg)
  if (!(name %in% all.supported.db)) {
    stop(sprintf("%s not be supported.", name))
  }
  config <- configr::read.config(database.cfg)
  config <- config[names(config) != "Title"]
  index <- lapply(config, function(x) {
    name %in% x[["versions"]]
  })
  index <- unlist(index)
  config <- config[[names(config)[index]]]
  return(config$func)
}

# Set dbname in annotation.R annotation.pos.utils
dbname.initial <- function(name, dbname.fixed = NULL, setdb.fun = NULL, buildver = "hg19", 
  database.dir = NULL, db.type = "sqlite", db.file.prefix = "txt", mysql.connect.params = NULL, 
  sqlite.connect.params = NULL) {
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
    if ("db.file.prefix" %in% setdb.fun.args) {
      setdb.fun.params <- config.list.merge(setdb.fun.params, list(db.file.prefix = db.file.prefix))
    }
    dbname <- do.call(setdb.fun, setdb.fun.params)
  } else {
    dbname <- dbname.fixed
  }
  return(dbname)
}

# print.db.info Print the database info
print.db.info <- function(dbname, db.type, mysql.connect.params, verbose = TRUE) {
  if (db.type != "mysql" && !file.exists(dbname)) {
    stop(sprintf("%s database not existed, please check the database dir or setdb.fun function again.", 
      dbname))
  }
  if (db.type != "mysql") {
    info.msg(sprintf("Database path:%s", dbname), verbose = verbose)
  } else {
    info.msg(sprintf("Host: %s, Database:%s", mysql.connect.params$host, dbname), 
      verbose = verbose)
  }
}

# Initial table name
table.name.initial <- function(name, table.name.fixed, buildver, set.table.fun) {
  if (is.null(table.name.fixed)) {
    set.table.fun.params <- list(name = name, buildver = buildver)
    table.name <- do.call(set.table.fun, set.table.fun.params)
  } else {
    table.name <- table.name.fixed
  }
}

# Initial databae connect params
database.params.initial <- function(db.type, dbname = "", table.name = "", sqlite.connect.params = list(), 
  mysql.connect.params = list()) {
  if (db.type == "sqlite") {
    sqlite.connect.params$dbname <- dbname
    sqlite.connect.params$table.name <- table.name
  } else if (db.type == "mysql") {
    mysql.connect.params$dbname <- dbname
    mysql.connect.params$table.name <- table.name
  }
  return(list(sqlite = sqlite.connect.params, mysql = mysql.connect.params))
}

# Merge selected data and input data and get final output
get.full.match.final.table <- function(dat, selected.db.tb, matched.cols = "", selected.colnames = "", 
  verbose = FALSE) {
  # Generate a unique id to get final result according the input data table
  id <- 1:nrow(dat)
  dat <- cbind(dat, id)
  dat <- as.data.table(lapply(dat, function(x) as.character(x)))
  selected.db.tb <- as.data.table(lapply(selected.db.tb, function(x) as.character(x)))
  
  # Set data.table key to accelerate merge step
  keys <- paste0(matched.cols, collapse = "\",\"")
  text <- sprintf("setkey(dat, \"%s\")", keys)
  eval(paste0(text = text))
  text <- sprintf("setkey(selected.db.tb, \"%s\")", keys)
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
}

# Get Region matched cols
get.region.match.final.table <- function(dat, selected.db.tb, matched.cols = "", 
  selected.colnames = "", verbose = FALSE) {
  return(TRUE)
}

# Some of function to convert name

## ALL.2015.08 => name = hg19_ALL.sites.2015.08.txt, mongh = aug, year = 2015,
## region = all
convert.1000g.name <- function(name) {
  month.hash <- list(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", 
    jun = "06", jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")
  month <- str_extract(name, names(month.hash))
  month <- month[!is.na(month)]
  month <- month.hash[month]
  year <- str_extract(name, "1000g[0-9]*")
  year <- str_replace(year, "1000g", "")
  region <- str_extract(name, "_[a-z]*")
  region <- toupper(str_replace(region, "_", ""))
  return(list(name = name, month = month, year = year, region = region))
}
