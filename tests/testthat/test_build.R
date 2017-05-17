test_that("sqlite.build", {
  test.sqlite <- sprintf("%s/snp.test.sqlite", tempdir())
  test.dat <- system.file("extdata", "demo/sqlite.dat.txt", package = "annovarR")
  x <- sqlite.build(test.dat, test.sqlite, "snp_test")
  expect_that(x, equals(TRUE))
  unlink(test.sqlite)
})

test_that("sqlite.index", {
  test.sqlite <- sprintf("%s/snp.test.sqlite", tempdir())
  test.dat <- system.file("extdata", "demo/sqlite.dat.txt", package = "annovarR")
  x <- sqlite.build(test.dat, test.sqlite, "snp_test")
  x <- sqlite.index(test.sqlite, "snp_test", "index4", c("V1", "V2"))
  expect_that(is.logical(x), equals(FALSE))
  x <- sqlite.index(test.sqlite, "snp_test", "index")
  expect_that(x, equals(FALSE))
  unlink(test.sqlite)
})

test_that("drop.sqlite.index", {
  test.sqlite <- sprintf("%s/snp.test.sqlite", tempdir())
  test.dat <- system.file("extdata", "demo/sqlite.dat.txt", package = "annovarR")
  x <- sqlite.build(test.dat, test.sqlite, "snp_test")
  x <- sqlite.index(test.sqlite, "snp_test", "index4", c("V1", "V2"))
  x <- drop.sqlite.index(test.sqlite, "index4")
  expect_that(is.logical(x), equals(FALSE))
  unlink(test.sqlite)
})
