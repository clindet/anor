database.dir <- tempdir()
for (i in c("hg19_avsnp147", "hg19_cscd_cancer_circrna")) {
  # database <- system.file('extdata', sprintf('demo/sqlite/%s.sqlite', i), package
  # = 'annovarR') file.copy(database, sprintf('%s/%s.sqlite', tempdir(), i))
  # database <- system.file('extdata', sprintf('demo/%s.txt', i), package =
  # 'annovarR') file.copy(database, sprintf('%s/%s.txt', tempdir(), i))
  database <- system.file("extdata", sprintf("demo/%s.txt", i), package = "annovarR")
  sqlite.db <- sprintf("%s/%s.sqlite", tempdir(), i)
  file.copy(database, sprintf("%s/%s.txt", tempdir(), i))
  sqlite.build(database, sqlite.connect.params = list(dbname = sqlite.db, table.name = sprintf("%s", 
    i)))
}

test_that("annotation.cols.match", {
  chr <- c("chr1", "chr2", "chr1")
  start <- c("10020", "10020", "10020")
  end <- c("10020", "10020", "10020")
  ref <- c("A", "A", "A")
  alt <- c("-", "-", "-")
  dat <- data.table(chr = chr, start = start, end = end, ref = ref, alt = alt)
  x <- annotation.cols.match(dat, "avsnp147", database.dir = database.dir, return.col.names = "avSNP147")
  x <- as.data.frame(x)
  expect_that(colnames(x), equals("avSNP147"))
  x[, 1] <- as.character(x[, 1])
  expect_that(x[1, 1], equals("rs775809821"))
  expect_that(is.na(x[2, 1]), equals(TRUE))
  expect_that(x[3, 1], equals("rs775809821"))
  x <- annotation.cols.match(dat, "avsnp147", database.dir = database.dir, return.col.names = "avSNP147", 
    db.type = "txt")
  x <- as.data.frame(x)
  expect_that(colnames(x), equals("avSNP147"))
  x[, 1] <- as.character(x[, 1])
  expect_that(x[1, 1], equals("rs775809821"))
  expect_that(is.na(x[2, 1]), equals(TRUE))
  expect_that(x[3, 1], equals("rs775809821"))
})

test_that("annotation.snp", {
  chr <- c("chr1", "chr2", "chr1")
  start <- c("10020", "10020", "10020")
  end <- c("10020", "10020", "10020")
  ref <- c("A", "A", "A")
  alt <- c("-", "-", "-")
  database.dir <- tempdir()
  dat <- data.table(chr = chr, start = start, end = end, ref = ref, alt = alt)
  x <- annotation(dat = dat, anno.name = "avsnp147", database.dir = database.dir)
  x <- as.data.frame(x)
  expect_that(colnames(x), equals("avSNP147"))
  x[, 1] <- as.character(x[, 1])
  expect_that(x[1, 1], equals("rs775809821"))
  expect_that(is.na(x[2, 1]), equals(TRUE))
  expect_that(x[3, 1], equals("rs775809821"))
  
})

for (i in c("hg19_avsnp147", "hg19_cscd_cancer_circrna")) {
  sqlite.db <- sprintf("%s/%s.sqlite", tempdir(), i)
  txt.db <- sprintf("%s/%s.txt", tempdir(), i)
  sqlite.db <- normalizePath(sqlite.db, "/")
  txt.db <- normalizePath(txt.db, "/")
  file.remove(sqlite.db)
  file.remove(txt.db)
}
