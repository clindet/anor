test_that("sqlite.build", {
  test.sqlite <- tempfile()
  test.dat <- system.file("extdata", "demo/sqlite.dat.txt", package = "annovarR")
  x <- sqlite.build(test.dat, list(sqlite.path = test.sqlite, table.name = "snp_test"))
  expect_that(x, equals(TRUE))
  unlink(test.sqlite)
})

test_that("sqlite.index", {
  test.sqlite <- tempfile()
  test.dat <- system.file("extdata", "demo/sqlite.dat.txt", package = "annovarR")
  x <- sqlite.build(test.dat, list(sqlite.path = test.sqlite, table.name = "snp_test"))
  x <- sqlite.index(list(sqlite.path = test.sqlite, table.name = "snp_test"), "index4", 
    c("V1", "V2"))
  expect_that(is.logical(x), equals(FALSE))
  x <- sqlite.index(list(sqlite.path = test.sqlite, table.name = "snp_test"), "index")
  expect_that(x, equals(FALSE))
  unlink(test.sqlite)
})

test_that("drop.sqlite.index", {
  test.sqlite <- tempfile()
  test.dat <- system.file("extdata", "demo/sqlite.dat.txt", package = "annovarR")
  x <- sqlite.build(test.dat, list(sqlite.path = test.sqlite, table.name = "snp_test"))
  x <- sqlite.index(list(sqlite.path = test.sqlite, table.name = "snp_test"), "index4", 
    c("V1", "V2"))
  x <- drop.sqlite.index(list(sqlite.path = test.sqlite), "index4")
  expect_that(is.logical(x), equals(FALSE))
  unlink(test.sqlite)
})

test_that("del", {
  db <- tempfile()
  db <- normalizePath(db, "/", mustWork = FALSE)
  file.create(db)
  x <- del(db, del.type = "file")
  expect_that(file.exists(db), equals(FALSE))
  test.dat <- system.file("extdata", "demo/sqlite.dat.txt", package = "annovarR")
  test.sqlite <- tempfile()
  test.sqlite <- normalizePath(test.sqlite, "/", mustWork = FALSE)
  x <- sqlite.build(filename = test.dat, list(sqlite.path = test.sqlite, table.name = "snp_test"))
  expect_that(file.exists(test.sqlite), equals(TRUE))
  params <- list(sqlite.connect.params = list(sqlite.path = test.sqlite, table.name = "snp_test"), 
    del.type = "table")
  result <- do.call(del, params)
  x <- del(sqlite.connect.params = list(sqlite.path = test.sqlite, table.name = "snp_test"), 
    del.type = "file")
  expect_that(x, equals(TRUE))
  expect_that(file.exists(test.sqlite), equals(FALSE))
  x <- sqlite.build(filename = test.dat, list(sqlite.path = test.sqlite, table.name = "snp_test"))
  x <- del(sqlite.connect.params = list(sqlite.path = test.sqlite, table.name = "snp_test"), 
    del.type = "database")
  expect_that(x, equals(TRUE))
})
