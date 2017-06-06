filename = system.file("extdata", "demo/hg19_ALL.sites.2015_08.txt", package = "annovarR")

test_that("format.1000g.db.tb", {
  x <- format.1000g.db.tb(filename = filename)
  x <- as.data.frame(x)
  cnames <- c("chr", "start", "end", "ref", "alt", "rs", "frq")
  expect_that(colnames(x), equals(cnames))
  expect_that(x["5", "end"], equals(10352))
  value <- c("10637", "CGCCGTTGCAAAGGCGCGCCG", "-", "rs376342519", "0.993011")
  expect_that(as.character(x[14, 3:7]), equals(value))
  value <- c("10235", "-", "A", "rs540431307", "0.00119808")
  expect_that(as.character(x[3, 3:7]), equals(value))
  value <- c("10352", "T", "TA", "rs555500075", "0.4375")
  expect_that(as.character(x[6, 3:7]), equals(value))
})

test_that("set.1000g.db", {
  x <- set.1000g.db("1000g2015aug_all", "hg19", "/db", "sqlite")
  expect_that(x, equals("/db/hg19_ALL.sites.2015_08.sqlite"))
  x <- set.1000g.db("1000g2015aug_all", "hg19", "/db", "txt")
  expect_that(x, equals("/db/hg19_ALL.sites.2015_08.txt"))
  x <- set.1000g.db("1000g2015aug_all", "hg19", "/db", "mysql")
  expect_that(is.null(x), equals(TRUE))
})

