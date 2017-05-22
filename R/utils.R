info.msg <- function(msg, verbose = FALSE, ...) {
  if (verbose) {
    flog.info(msg, ...)
  }
}

print.vb <- function(x, verbose = FALSE, ...) {
  if (verbose) {
    print(x)
  }
}


db.tb.colnames <- function(db.path, table.name, db.type = "sqlite") {
  if (db.type == "sqlite") {
    tb.colname <- sqlite.tb.colnames(db.path, table.name)
  } else if (db.type == "txt") {
    table.dat <- fread(db.path, nrows = 1)
    tb.colnames <- colnames(table.dat)
  }
}

connect.db <- function(db, db.type = "sqlite") {
  if (db.type == "sqlite") {
    db <- dbConnect(RSQLite::SQLite(), db)
  } else if (db.type == "txt") {
    return(db)
  }
}
disconnect.db <- function(db, db.type = "sqlite") {
  if (db.type == "sqlite") {
    dbDisconnect(db)
  }
}


get.input.index <- function(dat.list, matched.cols) {
  return(get.index(dat.list, matched.cols))
}

get.ref.index <- function(dat, matched.cols) {
  return(get.index(dat, matched.cols))
}

get.index <- function(dat, matched.cols = c()) {
  matched.cols.text <- NULL
  index <- NULL
  for (i in 1:length(matched.cols)) {
    if (i < length(matched.cols)) {
      matched.cols.text <- paste0(matched.cols.text, sprintf("dat$`%s`, ", 
        matched.cols[i]))
    } else {
      matched.cols.text <- paste0(matched.cols.text, sprintf("dat$`%s`", matched.cols[i]))
    }
  }
  text <- sprintf("index <- paste0(%s)", matched.cols.text)
  eval(parse(text = text))
  return(index)
}

sync.colnames <- function(result, col.order, col.names) {
  colnames(result)[col.order] <- col.names
  return(result)
}

select.dat <- function(db, table.name, cols = c(), params = list(), db.type = "sqlite", 
  select.cols = "*", verbose = FALSE) {
  params <- lapply(params, function(x) {
    as.character(x)
  })
  if (db.type == "sqlite") {
    sql <- sprintf("SELECT %s FROM \"%s\"", select.cols, table.name)
    if (length(cols) == 0) {
      result <- dbGetQuery(db, sql)
    } else {
      sql <- paste0(sql, " WHERE ")
      count <- 1
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
    keys <- paste0(names(params), collapse = ", ")
    text <- sprintf("setkey(result, %s)", keys)
    eval(parse(text = text))
    params <- as.data.table(params)
    keys <- paste0(names(params), collapse = ", ")
    text <- sprintf("setkey(params, %s)", keys)
    eval(parse(text = text))
    result <- merge(result, params)
  }
  return(result)
}

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

