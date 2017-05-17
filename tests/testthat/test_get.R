test_that("head.db", {
  test.sqlite <- sprintf("%s/snp.test.sqlite", tempdir())
  test.dat <- system.file("extdata", "demo/sqlite.dat.txt", package = "annovarR")
  x <- sqlite.build(filename = test.dat, db = test.sqlite, table.name = "snp_test")
  dat <- sqlite.head(test.sqlite, "snp_test", n = 5)
  expect_that(nrow(dat), equals(5))
})
