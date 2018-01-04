test_that("sqlite.head", {
  test.sqlite <- sprintf("%s/snp.test.sqlite", tempdir())
  test.dat <- system.file("extdata", "demo/sqlite.dat.txt", package = "annovarR")
  x <- sqlite.build(filename = test.dat, list(dbname = test.sqlite, table.name = "snp_test"))
  dat <- sqlite.head(list(dbname = test.sqlite, table.name = "snp_test"), n = 5)
  expect_that(nrow(dat), equals(5))
  unlink(test.sqlite)
})

test_that("get.cfg", {
  cfg <- system.file("extdata", "config/config.toml", package = "annovarR")
  x <- get.annotation.names()
  expect_that("avsnp138" %in% x, equals(TRUE))
  
  x <- get.annotation.dbtype("avsnp147")
  expect_that(x, equals("sqlite"))
  x <- get.annotation.dbtype("2016sih_wes_ball")
  expect_that(x, equals("txt"))
})

test_that("get.download.name", {
  x <- get.download.name("avsnp147")
  expect_that(x, equals("db_annovar_avsnp_sqlite"))
})

test_that("get.annotation.needcols", {
  x <- get.annotation.needcols("avsnp147")
  expect_that(x[1], equals("chr"))
})
