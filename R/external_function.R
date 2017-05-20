# annovarR default function: setdb.fun, set.table.fun, format.db.tb.fun You can
# re-write this R source file
set.db <- function(name, builder, database.dir, db.type) {
  if (db.type == "sqlite") {
    db.path <- sprintf("%s/%s_%s.%s", database.dir, builder, name, db.type)
  } else if (db.type == "txt") {
    db.path <- sprintf("%s/%s_%s.%s", database.dir, builder, name, db.type)
  }
  return(db.path)
}
set.table <- function(name, builder) {
  table.name <- paste0(builder, "_", name)
}

format.db.tb <- function(db.tb) {
  return(db.tb)
}

# 1000G needed functions to set database name and table name
set.1000g.db <- function(name, builder, database.dir, db.type = "sqlite") {
  list.1000g <- convert.1000g.name(name)
  if (db.type == "sqlite") {
    db <- sprintf("%s/%s_%s.sites.%s_%s.sqlite", database.dir, builder, list.1000g$region, 
      list.1000g$year, list.1000g$month)
  } else if (db.type == "txt") {
    db <- sprintf("%s/%s_%s.sites.%s_%s.txt", database.dir, builder, list.1000g$region, 
      list.1000g$year, list.1000g$month)
  }
}
set.1000g.table <- function(name, builder) {
  list.1000g <- convert.1000g.name(name)
  table <- sprintf("%s_%s.sites.%s_%s", builder, list.1000g$region, list.1000g$year, 
    list.1000g$month)
}

format.1000g.db.tb <- function(dat = "", filename = "", ...) {
  if (filename != "") {
    dat <- fread(filename, ...)
  }
  dat <- as.data.frame(dat)
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


# Sih Normal Pool needed functions to set database name and table name
set.sih.normal.pool.db <- function(name, builder, database.dir, db.type = "txt") {
  if (db.type == "sqlite") {
    db <- sprintf("%s/%s_normal%s.sqlite", database.dir, builder, name)
  } else if (db.type == "txt") {
    db <- sprintf("%s/%s_normal%s.txt", database.dir, builder, name)
  }
}
