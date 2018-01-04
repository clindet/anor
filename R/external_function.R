# annovarR default function: setdb.fun, set.table.fun, format.db.tb.fun You can
# re-write this R source file
format.cols <- function(dat.input = "") {
  dat.input <- dat.input
  if ("chr" %in% names(dat.input)) {
    if (str_detect(dat.input[["chr"]][1], "chr|Chr")) {
      dat.input$chr <- str_replace(as.character(dat.input$chr), "chr|Chr", 
        "")
    }
  }
  return(dat.input)
}

# Some of database format of Chr col have 'chr' flag
format.cols.plus.chr <- function(dat.input = "") {
  if (!str_detect(dat.input$chr[1], "chr|Chr|CHR")) {
    dat.input$chr <- paste0("chr", dat.input$chr)
  } else if (str_detect(dat.input$chr[1], "Chr|CHR")) {
    dat.input$chr <- str_replace_all(dat.input$chr, "Chr|CHR", "chr")
  }
  return(dat.input)
}

# Default set database name function
set.db <- function(anno.name = "", buildver = "hg19", database.dir = "", db.type = "", 
  db.file.prefix = NULL, mysql.connect.params = list(), sqlite.connect.params = list()) {
  if (is.null(db.file.prefix)) {
    db.file.prefix <- db.type
  }
  if (db.type %in% c("sqlite", "txt")) {
    if (buildver != "") {
      dbname <- sprintf("%s/%s_%s.%s", database.dir, buildver, anno.name, db.file.prefix)
    } else {
      dbname <- sprintf("%s/%s.%s", database.dir, anno.name, db.file.prefix)
    }
  } else if (db.type == "mysql") {
    if (is.null(mysql.connect.params$dbname)) {
      dbname <- sprintf("%s_%s", buildver, anno.name)
    } else {
      dbname <- mysql.connect.params[["dbname"]]
    }
  }
  return(dbname)
}

set.db.toupper <- function(...) {
  params <- list(...)
  params$anno.name <- toupper(params$anno.name)
  do.call(set.db, params)
}

set.db.tolower <- function(...) {
  params <- list(...)
  params$anno.name <- tolower(params$anno.name)
  do.call(set.db, params)
}

set.db.sync.version <- function(...) {
  params <- list(...)
  params$buildver <- ""
  do.call(set.db, params)
}

# Default set table name function
set.table <- function(anno.name = "", buildver = "", db.type = "sqlite", mysql.connect.params = list(), 
  sqlite.connect.params = list()) {
  if (buildver != "") {
    table.name <- paste0(buildver, "_", anno.name)
  } else {
    table.name <- anno.name
  }
}

set.table.tolower <- function(...) {
  params <- list(...)
  params$anno.name <- tolower(params$anno.name)
  do.call(set.table, params)
}

set.table.toupper <- function(...) {
  params <- list(...)
  params$anno.name <- toupper(params$anno.name)
  do.call(set.table, params)
}

set.table.sync.version <- function(...) {
  params <- list(...)
  params$buildver <- ""
  do.call(set.table, params)
}

format.db.tb <- function(...) {
  params <- list(...)
  return(params$db.tb)
}

# Format selected data table family: rs2pos
format.db.tb.unique <- function(...) {
  params <- list(...)
  db.tb <- params$db.tb
  if (nrow(db.tb) == 0) {
    return(db.tb)
  }
  tb.matched.cols <- params$tb.matched.cols
  if (is.null(tb.matched.cols)) {
    stop("Error in format.db.tb.unique: not set matched.cols using database colnames.")
  }
  tb.matched.cols.raw <- tb.matched.cols
  
  need.cols <- colnames(db.tb)
  is.region <- !is.null(params$input.dat) && !is.null(params$inferior.col)
  if (is.region) {
    # region match, according xid to merge mulitple row to one
    index.table <- full.foverlaps(db.tb, params$input.dat, params$full.matched.cols, 
      params$inferior.col, params$superior.col)$index.table
    db.tb <- db.tb[index.table$yid, ]
    db.tb <- cbind(db.tb, index.table[, 1])
    db.tb <- cbind(db.tb, index.table[, 2])
    tb.matched.cols <- "xid"
  }
  need.cols <- need.cols[!need.cols %in% tb.matched.cols.raw]
  keys <- paste0(tb.matched.cols, collapse = "\", \"")
  text <- sprintf("setkey(db.tb, \"%s\")", keys)
  eval(parse(text = text))
  text <- sprintf("db.tb[, new:=paste0(%s)]", paste0(tb.matched.cols, collapse = ", "))
  eval(parse(text = text))
  rs.frq <- table(db.tb$new)
  rs.frq <- as.data.table(rs.frq)
  for (i in rs.frq$V1[rs.frq$N > 1]) {
    first.line <- which(db.tb$new == i)[1]
    for (j in need.cols) {
      db.tb[[j]][first.line] <- paste0(db.tb[[j]][db.tb$new == i], collapse = "//")
    }
  }
  text <- sprintf("db.tb <- db.tb[,-%s]", ncol(db.tb))
  eval(parse(text = text))
  if (is.region) {
    text <- sprintf("db.tb <- db.tb[,-%s]", ncol(db.tb))
    eval(parse(text = text))
  }
  return(db.tb)
}

format.db.region.tb <- function(...) {
  params <- list(...)
  db.tb <- params$db.tb
  index.table <- full.foverlaps(db.tb, params$input.dat, params$full.matched.cols, 
    params$inferior.col, params$superior.col)$index.table
  db.tb <- db.tb[index.table$yid, ]
  db.tb <- cbind(db.tb, index.table[, 1])
  return(db.tb)
}

# 1000G needed functions to set database name and table name
set.1000g.db <- function(anno.name = "", buildver = "hg19", database.dir = "", db.type = "sqlite") {
  list.1000g <- convert.1000g.name(anno.name)
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
set.1000g.table <- function(anno.name = "", buildver = "hg19") {
  list.1000g <- convert.1000g.name(anno.name)
  table <- sprintf("%s_%s.sites.%s_%s", buildver, list.1000g$region, list.1000g$year, 
    list.1000g$month)
}

format.1000g.db.tb <- function(db.tb = "", filename = "", ...) {
  if (filename != "") {
    db.tb <- fread(filename, ...)
  }
  end <- as.numeric(db.tb$V2)
  have.num <- str_detect(db.tb$V4, "[0-9]")
  end[have.num] <- end[have.num] + as.numeric(str_extract(db.tb$V4[have.num], "[0-9]*"))
  db.tb <- cbind(db.tb[, c(1, 2)], end, db.tb[, c(3, 4, 6, 5)])
  only.num <- !str_detect(db.tb$V4, "[atcgATCG]")
  db.tb$end[only.num] <- db.tb$end[only.num] - 1
  db.tb$V4[only.num] <- "-"
  zero.alt <- str_detect(db.tb$V4, "0[atctATCG]*")
  db.tb$V4[zero.alt] <- str_replace(db.tb$V4[zero.alt], "0", "")
  db.tb$V4[zero.alt] <- str_replace(db.tb$V4[zero.alt], db.tb$V3[zero.alt], "")
  db.tb$V3[zero.alt] <- "-"
  have.num.but.no.only <- str_detect(db.tb$V4, "[1-9]")
  have.num.but.no.only <- have.num.but.no.only & !only.num
  db.tb$end[have.num.but.no.only] <- db.tb$end[have.num.but.no.only] - 1
  num <- str_extract(db.tb$V4, "[1-9]*")
  db.tb$V4[have.num.but.no.only] <- str_replace_all(db.tb$V4[have.num.but.no.only], 
    "[0-9]*", "")
  ref.len <- str_length(db.tb$V3)
  alt.len <- str_length(db.tb$V4)
  is.deletion <- ref.len > alt.len
  colnames(db.tb) <- c("chr", "start", "end", "ref", "alt", "rs", "frq")
  return(db.tb)
}

# Sih Normal Pool needed functions to set database name and table name
set.sih.normal.pool.db <- function(anno.name = "", buildver = "hg19", database.dir = "", 
  db.type = "txt") {
  if (db.type == "sqlite") {
    db <- sprintf("%s/%s_normal%s.sqlite", database.dir, buildver, anno.name)
  } else if (db.type == "txt") {
    db <- sprintf("%s/%s_normal%s.txt", database.dir, buildver, anno.name)
  }
}

### rs2pos section ###
set.db.rs2pos <- function(...) {
  params <- list(...)
  params$anno.name <- str_replace(params$anno.name, "rs2pos", "avsnp")
  do.call(set.db, params)
}
set.table.rs2pos <- function(...) {
  params <- list(...)
  params$anno.name <- str_replace(params$anno.name, "rs2pos", "avsnp")
  do.call(set.table, params)
}

### refGene section ###
set.db.refgene <- function(...) {
  params <- list(...)
  if (params$anno.name == "ucsc_refgene") {
    params$anno.name <- "refGene"
  } else {
    params$anno.name <- "ensGene"
  }
  do.call(set.db, params)
}
set.table.refgene <- function(...) {
  params <- list(...)
  if (params$anno.name == "ucsc_refgene") {
    params$anno.name <- "refGene"
  } else {
    params$anno.name <- "ensGene"
  }
  do.call(set.table, params)
}

### hgnc section ###
set.db.hgnc.alias <- function(...) {
  params <- list(...)
  params$anno.name <- str_replace(params$anno.name, "_gene2pre|_pre2gene", "")
  do.call(set.db, params)
}
set.table.hgnc.alias <- function(...) {
  params <- list(...)
  params$anno.name <- str_replace(params$anno.name, "_gene2pre|_pre2gene", "")
  do.call(set.table, params)
}
