test_that("rs2genomic", {
  snp.id <- c("rs775809821", "rs768019142")
  raw.database <- system.file("extdata", "demo/hg19_avsnp147.txt", package = "annovarR")
  database.dir <- tempdir()
  sqlite.db <- sprintf("%s/hg19_avsnp147.sqlite", tempdir())
  txt.db <- sprintf("%s/hg19_avsnp147.txt", tempdir())
  file.copy(raw.database, txt.db)
  sqlite.build(raw.database, sqlite.connect.params = list(dbname = sqlite.db, table.name = "hg19_avsnp147"))
  # Custom
  set.table.custom <- function(x) {
    return(x)
  }
  x <- annotation.cols.match(dat = data.table(snp.id = snp.id), dbname.fixed = txt.db, 
    table.name.fixed = "hg19_avsnp147", db.col.order = 6, index.cols = "snp.id", 
    matched.cols = "snp.id", return.col.index = c(1, 2, 3, 4, 5), set.table.fun = set.table.custom, 
    verbose = FALSE, database.dir = database.dir, format.db.tb.fun = format.db.tb.unique, 
    db.type = "txt")
  expect_that(colnames(x), equals(paste0("V", 1:5)))
  x <- as.data.frame(x)
  expect_that(x[1, 1], equals("1//1"))
  expect_that(x[1, 2], equals("10019//10020"))
  expect_that(x[1, 4], equals("TA//A"))
  expect_that(x[1, 5], equals("T//-"))
  x <- annotation.cols.match(dat = data.table(snp.id = snp.id), dbname.fixed = sqlite.db, 
    table.name.fixed = "hg19_avsnp147", db.col.order = 6, index.cols = "snp.id", 
    matched.cols = "snp.id", return.col.index = c(1, 2, 3, 4, 5), set.table.fun = set.table.custom, 
    verbose = FALSE, database.dir = database.dir, format.db.tb.fun = format.db.tb.unique, 
    db.type = "sqlite")
  expect_that(colnames(x), equals(paste0("V", 1:5)))
  x <- as.data.frame(x)
  expect_that(x[1, 1], equals("1//1"))
  expect_that(x[1, 2], equals("10019//10020"))
  expect_that(x[1, 4], equals("TA//A"))
  expect_that(x[1, 5], equals("T//-"))
  
  # Auto set params
  x <- annotation(dat = data.table(rs = rep(snp.id, 3)), database.dir = database.dir, 
    anno.name = "rs2pos147", buildver = "hg19", verbose = FALSE, db.type = "txt")
  x <- as.data.frame(x)
  expect_that(colnames(x), equals(c("chr", "start", "end", "ref", "alt")))
  expect_that(x[1, 1], equals("1//1"))
  expect_that(x[1, 2], equals("10019//10020"))
  expect_that(x[1, 4], equals("TA//A"))
  expect_that(x[1, 5], equals("T//-"))
  x <- annotation(dat = data.table(rs = snp.id), database.dir = database.dir, anno.name = "rs2pos147", 
    buildver = "hg19", verbose = FALSE, db.type = "sqlite")
  x <- as.data.frame(x)
  expect_that(colnames(x), equals(c("chr", "start", "end", "ref", "alt")))
  expect_that(x[1, 1], equals("1//1"))
  expect_that(x[1, 2], equals("10019//10020"))
  expect_that(x[1, 4], equals("TA//A"))
  expect_that(x[1, 5], equals("T//-"))
})
