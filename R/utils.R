# Add verbose to decide wheather using flog.info infomation
info.msg <- function(msg = "", verbose = FALSE, ...) {
  if (verbose) {
    flog.info(msg, ...)
  }
}

# Add verbose to decide wheather print infomation
print.vb <- function(x = "", verbose = FALSE, ...) {
  if (verbose) {
    print(x)
  }
}

# Connect API for sqlite and mysql database, text file will return db dbname for
# sqlite and txt database
connect.db <- function(dbname = "", db.type = "sqlite", sqlite.connect.params = list(), 
  mysql.connect.params = list(), verbose = TRUE) {
  if (db.type == "mysql") {
    info.msg(sprintf("Setting up connection: Host:%s databse:%s.", mysql.connect.params$host, 
      mysql.connect.params$dbname), verbose = verbose)
  } else if (db.type == "sqlite") {
    info.msg(sprintf("Setting up connection: %s sqlite databse.", sqlite.connect.params$dbname), 
      verbose = verbose)
  } else {
    info.msg(sprintf("Setting up connection: %s databse.", dbname), verbose = verbose)
  }
  if (db.type == "sqlite") {
    sqlite.connect.params <- config.list.merge(list(RSQLite::SQLite()), sqlite.connect.params)
    database <- do.call(dbConnect, sqlite.connect.params)
  } else if (db.type == "txt") {
    database <- dbname
  } else if (db.type == "mysql") {
    mysql.connect.params <- config.list.merge(list(MySQL()), mysql.connect.params)
    database <- do.call(dbConnect, mysql.connect.params)
    dbSendQuery(database, "SET NAMES utf8")
  }
  return(database)
}
# DisConnect API to avoid the error that db.type not is sqlite or mysql
disconnect.db <- function(database = "", db.type = "sqlite") {
  if (db.type == "sqlite" || db.type == "mysql") {
    dbDisconnect(database)
  }
}

# Can be used to get the value from database.cfg, `name` is one of the first
# level name of database.cfg, `key` is the key of first level name, `coincident`
# decide wheather using one value to reprenst all of version
get.cfg.value.by.name <- function(name = "", database.cfg = system.file("extdata", 
  "config/databases.toml", package = "annovarR"), key = "", coincident = FALSE, 
  extra.list = list(), rcmd.parse = TRUE, glue.parse = TRUE) {
  config <- configr::read.config(database.cfg, extra.list = extra.list, rcmd.parse = rcmd.parse, 
    glue.parse = glue.parse)
  config <- config[names(config) != "Title"]
  index <- lapply(config, function(x) {
    name %in% x[["versions"]]
  })
  index <- unlist(index)
  config <- config[[names(config)[index]]]
  if (!key %in% names(config)) {
    return(NULL)
  }
  if (coincident) {
    if (!key %in% names(config)) {
      return(NULL)
    }
    return(config[[key]])
  } else {
    index <- name == config$versions
    return(config[[key]][index])
  }
}
