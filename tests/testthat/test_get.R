test_that("sqlite.head", {
  test.sqlite <- sprintf("%s/snp.test.sqlite", tempdir())
  test.dat <- system.file("extdata", "demo/sqlite.dat.txt", package = "annovarR")
  x <- sqlite.build(filename = test.dat, db = test.sqlite, table.name = "snp_test")
  dat <- sqlite.head(test.sqlite, "snp_test", n = 5)
  expect_that(nrow(dat), equals(5))
})



test_that("show.cfg.databses", {
  cfg <- system.file("extdata", "config/config.toml", package = "annovarR")
  x <- show.cfg.databses()
  expect_that("avsnp138" %in% x, equals(TRUE))
})
