test_that("rs2genomic", {
  snp.id <- c("rs775809821", "rs768019142")
  database <- system.file("extdata", "demo/hg19_avsnp147.txt", package = "annovarR")
  database.dir <- dirname(database)
  func <- function(dat) {
    setkey(dat, V6)
    rs.frq <- table(dat$V6)
    rs.frq <- as.data.table(rs.frq)
    for (i in rs.frq$V1[rs.frq$N > 1]) {
      first.line <- which(dat$V6 == i)[1]
      dat$V1[first.line] <- paste0(dat$V1[dat$V6 == i], collapse = ",")
      dat$V2[first.line] <- paste0(dat$V2[dat$V6 == i], collapse = ",")
      dat$V3[first.line] <- paste0(dat$V3[dat$V6 == i], collapse = ",")
      dat$V4[first.line] <- paste0(dat$V4[dat$V6 == i], collapse = ",")
      dat$V5[first.line] <- paste0(dat$V5[dat$V6 == i], collapse = ",")
    }
    return(dat)
  }
  # Custom
  set.table.custom <- function(x) {
    return(x)
  }
  x <- annotation.cols.match(dat = data.table(snp.id = snp.id), dbname.fixed = database, 
    table.name.fixed = "hg19_avsnp147", db.col.order = 6, index.cols = "snp.id", 
    matched.cols = "snp.id", return.col.index = c(1, 2, 3, 4, 5), set.table.fun = set.table.custom, 
    verbose = FALSE, database.dir = database.dir, format.db.tb.fun = func, db.type = "txt")
  expect_that(colnames(x), equals(paste0("V", 1:5)))
  x <- as.data.frame(x)
  expect_that(x[1,1], equals('1,1'))
  expect_that(x[1,2], equals('10019,10020'))
  expect_that(x[1,4], equals('TA,A'))
  expect_that(x[1,5], equals('T,-'))
  # Recode in database.cfg
  x <- annotation(dat = data.table(rs = snp.id), database.dir = database.dir, 
    name = "rs2pos147", buildver = "hg19", verbose = FALSE, db.type = "txt")
  x <- as.data.frame(x)
  expect_that(colnames(x), equals(c("chr", "start", "end", "ref", "alt")))
  expect_that(x[1,1], equals('1,1'))
  expect_that(x[1,2], equals('10019,10020'))
  expect_that(x[1,4], equals('TA,A'))
  expect_that(x[1,5], equals('T,-'))
})
