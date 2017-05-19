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

format.cols <- function(dat.list) {
  dat.list <- dat.list
  if ("chr" %in% names(dat.list)) {
    dat.list$chr <- str_replace(as.character(dat.list$chr), "chr", "")
  }
  if ("start" %in% names(dat.list)) {
    dat.list$start <- as.numeric(dat.list$start)
  }
  if ("end" %in% names(dat.list)) {
    dat.list$end <- as.numeric(dat.list$end)
  }
  return(dat.list)
}

format.db.tb <- function(db.tb) {
  return(db.tb)
}

db.tb.colnames <- function(db.path, table.name, db.type = "sqlite") {
  if (db.type == "sqlite") {
    tb.colname <- sqlite.tb.colnames(db.path, table.name)
  } else if (db.type == "txt") {
    table.dat <- as.data.frame(fread(db.path, nrows = 1))
    tb.colnames <- colnames(table.dat)
  }
}

set.db <- function(name, builder, database.dir, db.type) {
  if (db.type == "sqlite") {
    db.path <- sprintf("%s/%s_%s.%s", database.dir, builder, name, db.type)
  } else if (db.type == "txt") {
    db.path <- sprintf("%s/%s_%s.%s", database.dir, builder, name, db.type)
  }
  return(db.path)
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

set.table <- function(name, builder) {
  table.name <- paste0(builder, "_", name)
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
  if (db.type == "sqlite") {
    sql <- sprintf("SELECT %s FROM \"%s\"", select.cols, table.name)
    if (length(cols) == 0) {
      result <- dbGetQuery(db, sql)
    } else {
      sql <- paste0(sql, " WHERE ")
      count <- 1
      for (i in 1:length(params)) {
        if (i < length(params)) {
          sql.plus <- sprintf("\"%s\" == :x%s AND ", cols[i], i)
          sql <- paste0(sql, sql.plus)
        } else {
          sql.plus <- sprintf("\"%s\" == :x%s", cols[i], i)
          sql <- paste0(sql, sql.plus)
        }
      }
    }
    params <- unname(params)
    info.msg(sprintf("Quering sql: %s", sql), verbose = verbose)
    result <- dbGetQuery(db, sql, params = params)
    info.msg(sprintf("Finish query: %s", sql), verbose = verbose)
  } else if (db.type == "txt") {
    result <- as.data.frame(fread(db))
  }
  return(result)
}

return.empty.col <- function(dat.list, tb.colnames, return.col.index, return.col.names) {
  result <- rep(NA, length(dat.list[[1]]))
  result <- data.frame(result)
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
  name <- tolower(name)
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
  name <- tolower(name)
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
