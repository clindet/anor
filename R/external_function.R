# annovarR default function: setdb.fun, set.table.fun, format.db.tb.fun You can
# re-write this R source file
format.cols <- function(dat.input) {
  dat.input <- dat.input
  if ("chr" %in% names(dat.input)) {
    if (str_detect(dat.input[["chr"]][1], "chr|Chr")) {
      dat.input$chr <- str_replace(as.character(dat.input$chr), "chr|Chr", 
        "")
    }
  }
  return(dat.input)
}

set.db <- function(name, buildver = "hg19", database.dir = "", db.type = "", db.file.prefix = "txt", 
  mysql.connect.params = list(), sqlite.connect.params = list()) {
  if (db.type == "sqlite") {
    dbname <- sprintf("%s/%s_%s.%s", database.dir, buildver, name, db.file.prefix)
  } else if (db.type == "txt") {
    dbname <- sprintf("%s/%s_%s.%s", database.dir, buildver, name, db.file.prefix)
  } else if (db.type == "mysql") {
    if (is.null(sqlite.connect.params$dbname)) {
      dbname <- sprintf("%s_%s", buildver, name)
    } else {
      dbname <- mysql.connect.params[["dbname"]]
    }
  }
  return(dbname)
}
# Default set table name function
set.table <- function(name = "", buildver = "", db.type = "sqlite", mysql.connect.params = list(), 
  sqlite.connect.params = list()) {
  if (db.type == "sqlite") {
    table.name <- paste0(buildver, "_", name)
  } else if (db.type == "txt") {
    table.name <- paste0(buildver, "_", name)
  } else if (db.type == "mysql") {
    table.name <- paste0(buildver, "_", name)
  }
}

format.db.tb <- function(db.tb) {
  return(db.tb)
}

# 1000G needed functions to set database name and table name
set.1000g.db <- function(name, buildver, database.dir = "", db.type = "sqlite") {
  list.1000g <- convert.1000g.name(name)
  if (db.type != "mysql") {
    if (database.dir != "") {
      db <- sprintf("%s/%s_%s.sites.%s_%s.%s", database.dir, buildver, list.1000g$region, 
        list.1000g$year, list.1000g$month, db.type)
    } else {
      db <- sprintf("%s_%s.sites.%s_%s.%s", buildver, list.1000g$region, list.1000g$year, 
        list.1000g$month, db.type)
    }
  }
}
set.1000g.table <- function(name, buildver) {
  list.1000g <- convert.1000g.name(name)
  table <- sprintf("%s_%s.sites.%s_%s", buildver, list.1000g$region, list.1000g$year, 
    list.1000g$month)
}

format.1000g.db.tb <- function(dat = "", filename = "", ...) {
  if (filename != "") {
    dat <- fread(filename, ...)
  }
  end <- as.numeric(dat$V2)
  have.num <- str_detect(dat$V4, "[0-9]")
  end[have.num] <- end[have.num] + as.numeric(str_extract(dat$V4[have.num], "[0-9]*"))
  dat <- cbind(dat[, c(1, 2)], end, dat[, c(3, 4, 6, 5)])
  only.num <- !str_detect(dat$V4, "[atcgATCG]")
  dat$end[only.num] <- dat$end[only.num] - 1
  dat$V4[only.num] <- "-"
  zero.alt <- str_detect(dat$V4, "0[atctATCG]*")
  dat$V4[zero.alt] <- str_replace(dat$V4[zero.alt], "0", "")
  dat$V4[zero.alt] <- str_replace(dat$V4[zero.alt], dat$V3[zero.alt], "")
  dat$V3[zero.alt] <- "-"
  have.num.but.no.only <- str_detect(dat$V4, "[1-9]")
  have.num.but.no.only <- have.num.but.no.only & !only.num
  dat$end[have.num.but.no.only] <- dat$end[have.num.but.no.only] - 1
  num <- str_extract(dat$V4, "[1-9]*")
  dat$V4[have.num.but.no.only] <- str_replace_all(dat$V4[have.num.but.no.only], 
    "[0-9]*", "")
  ref.len <- str_length(dat$V3)
  alt.len <- str_length(dat$V4)
  is.deletion <- ref.len > alt.len
  colnames(dat) <- c("chr", "start", "end", "ref", "alt", "rs", "frq")
  return(dat)
}

# Some of database format of Chr col have 'chr' flag
format.cols.plus.chr <- function(dat.input) {
  if (!str_detect(dat.input$chr[1], "chr|Chr|CHR")) {
    dat.input$chr <- paste0("chr", dat.input$chr)
  } else if (str_detect(dat.input$chr[1], "Chr|CHR")) {
    dat.input$chr <- str_replace_all(dat.input$chr, "Chr|CHR", "chr")
  }
  return(dat.input)
}

# Sih Normal Pool needed functions to set database name and table name
set.sih.normal.pool.db <- function(name, buildver, database.dir, db.type = "txt") {
  if (db.type == "sqlite") {
    db <- sprintf("%s/%s_normal%s.sqlite", database.dir, buildver, name)
  } else if (db.type == "txt") {
    db <- sprintf("%s/%s_normal%s.txt", database.dir, buildver, name)
  }
}
