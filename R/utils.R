# Add verbose to decide wheather using flog.info infomation
info.msg <- function(msg, verbose = FALSE, ...) {
  if (verbose) {
    flog.info(msg, ...)
  }
}

# Add verbose to decide wheather print infomation
print.vb <- function(x, verbose = FALSE, ...) {
  if (verbose) {
    print(x)
  }
}

# Show colnames of table in database or text file
db.tb.colnames <- function(dbname = "", table.name = "", db.type = "sqlite", mysql.connect.params = list()) {
  if (db.type == "sqlite") {
    tb.colname <- sqlite.tb.colnames(list(sqlite.path = dbname, table.name = table.name))
  } else if (db.type == "txt") {
    table.dat <- fread(dbname, nrows = 1)
    tb.colnames <- colnames(table.dat)
  } else if (db.type == "mysql") {
    params <- list(dbname = dbname, table.name = table.name)
    params <- config.list.merge(params, mysql.connect.params)
    tb.colname <- do.call(mysql.tb.colnames, params)
  }
}

# Connect API for sqlite and mysql database, text file will return db
connect.db <- function(database, db.type = "sqlite", sqlite.connect.params = list(), 
  mysql.connect.params = list(host = "host", dbname = database, port = "3306", 
    user = "user", password = "password")) {
  if (db.type == "sqlite") {
    sqlite.connect.params <- config.list.merge(list(RSQLite::SQLite(), database), 
      sqlite.connect.params)
    database <- do.call(dbConnect, sqlite.connect.params)
  } else if (db.type == "txt") {
    database <- database
  } else if (db.type == "mysql") {
    mysql.connect.params <- config.list.merge(list(MySQL(), dbname = database), 
      mysql.connect.params)
    database <- do.call(dbConnect, mysql.connect.params)
    dbSendQuery(database, "SET NAMES utf8")
  }
  return(database)
}
# DisConnect API to avoid the error that db.type not is sqlite or mysql
disconnect.db <- function(database, db.type = "sqlite") {
  if (db.type == "sqlite" || db.type == "mysql") {
    dbDisconnect(database)
  }
}

# Sync database and input table colnames
sync.colnames <- function(result, col.order, col.names) {
  colnames(result)[col.order] <- col.names
  return(result)
}

# Select data from text file, sqlite or mysql database cols: database colnames
# used to match params: a list that record to match database using cols
select.dat <- function(db, table.name, cols = c(), params = list(), db.type = "sqlite", 
  select.cols = "*", verbose = FALSE) {
  params <- lapply(params, function(x) {
    as.character(x)
  })
  if (db.type == "sqlite") {
    sql <- sprintf("SELECT %s FROM \"%s\"", select.cols, table.name)
    if (length(cols) > 0) {
      sql <- paste0(sql, " WHERE ")
      for (i in 1:length(params)) {
        if (i < length(params)) {
          sql.plus <- sprintf("\"%s\"==:x%s AND ", cols[i], i)
          sql <- paste0(sql, sql.plus)
        } else {
          sql.plus <- sprintf("\"%s\"==:x%s", cols[i], i)
          sql <- paste0(sql, sql.plus)
        }
      }
    }
    info.msg(sprintf("Input %s colnum type:%s", paste0(names(params), collapse = ","), 
      paste0(sapply(params, typeof), collapse = ",")), verbose = verbose)
    print.vb(lapply(params, head), verbose = verbose)
    params <- unname(params)
    info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
    result <- dbGetQuery(db, sql, params = params)
    info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
    result <- as.data.table(result)
  } else if (db.type == "mysql") {
    sql <- sprintf("SELECT %s FROM %s", select.cols, table.name)
    if (length(cols) >= 0) {
      sql <- sprintf("SELECT %s FROM %s", select.cols, table.name)
      sql <- paste0(sql, " WHERE ")
      for (i in 1:length(params)) {
        if (i < length(params)) {
          tmp.pars <- paste0(params[[cols[i]]], collapse = "', '")
          tmp.pars <- sprintf("'%s'", tmp.pars)
          sql.plus <- sprintf("%s in (%s) AND ", cols[i], tmp.pars)
          sql <- paste0(sql, sql.plus)
        } else {
          tmp.pars <- paste0(params[[cols[i]]], collapse = "', '")
          tmp.pars <- sprintf("'%s'", tmp.pars)
          sql.plus <- sprintf("%s in (%s)", cols[i], tmp.pars)
          sql <- paste0(sql, sql.plus)
        }
      }
      info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
      result <- dbGetQuery(db, sql)
    } else {
      info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
      result <- dbGetQuery(db, sql)
      info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
    }
    result <- as.data.table(result)
  } else if (db.type == "txt") {
    result <- fread(db)
    result <- lapply(result, function(x) {
      as.character(x)
    })
    params <- lapply(params, function(x) {
      as.character(x)
    })
    result <- as.data.table(result)
    params <- as.data.table(params)
    index <- match(colnames(result), names(params))
    index <- index[!is.na(index)]
    colnames(result)[index] <- names(params)
    keys <- paste0(names(params), collapse = '", "')
    text <- sprintf('setkey(result, "%s")', keys)
    eval(parse(text = text))
    params <- as.data.table(params)
    keys <- paste0(names(params), collapse = '", "')
    text <- sprintf('setkey(params, "%s")', keys)
    eval(parse(text = text))
    result <- merge(result, params)
  }
  return(result)
}

# Return NA cols using ordered colnames
return.empty.col <- function(dat.list, tb.colnames, return.col.index, return.col.names) {
  result <- rep(NA, length(dat.list[[1]]))
  result <- as.data.table(result)
  if (return.col.names != "") {
    colnames(result) <- return.col.names
  } else {
    colnames(result) <- tb.colnames[return.col.index]
  }
  return(result)
}

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

# Can be used to get the value from database.cfg, `name` is one of the first
# level name of database.cfg, `key` is the key of first level name, `coincident`
# decide wheather using one value to reprenst all of version
get.cfg.value.by.name <- function(name, database.cfg = system.file("extdata", "config/databases.toml", 
  package = "annovarR"), key = "", coincident = FALSE, extra.list = list(), rcmd.parse = TRUE) {
  config <- configr::read.config(database.cfg, extra.list = extra.list, rcmd.parse = rcmd.parse)
  config <- config[names(config) != "Title"]
  index <- lapply(config, function(x) {
    name %in% x[["versions"]]
  })
  index <- unlist(index)
  config <- config[[names(config)[index]]]
  if (coincident) {
    return(config[[key]])
  } else {
    index <- name == config$versions
    return(config[[key]][index])
  }
}

cbind.ffdf2 <- function(d1, d2) {
  D1names <- colnames(d1)
  D2names <- colnames(d2)
  mergeCall <- do.call("ffdf", c(physical(d1), physical(d2)))
  colnames(mergeCall) <- c(D1names, D2names)
  mergeCall
}


# Sqlite connenct initial
sqlite.connect.initial <- function(sqlite.connect.params = list(sqlite.path = ""), verbose = FALSE){
  sqlite.path <- sqlite.connect.params[["sqlite.path"]]
  if (names(sqlite.connect.params)[1] != "") {
    sqlite.connect.params <- config.list.merge(list(sqlite.connect.params[["sqlite.path"]]), 
      sqlite.connect.params)
  }
  info.msg(sprintf("Setting up connection: %s sqlite databse.", sqlite.path), verbose = verbose)
  sqlite.connect.params <- config.list.merge(list(SQLite()), sqlite.connect.params)
  sqlite.db <- do.call(dbConnect, sqlite.connect.params)
}
