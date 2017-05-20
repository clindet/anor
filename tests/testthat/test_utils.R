database <- system.file("extdata", "demo/hg19_cosmic81.sqlite", package = "annovarR")
test_that("select.dat", {
  database <- dbConnect(RSQLite::SQLite(), database)
  x <- select.dat(database, "hg19_cosmic81", "V1", list("1"))
  expect_that(colnames(x), equals(paste0("V", 1:6)))
  expect_that(x[1, 2], equals(13496008))
})


test_that("get.annotation.func", {
  x <- get.annotation.func("cosmic70")
  expect_that(x, equals("annotation.auto"))
})


test_that("get.cfg.value.by.name", {
  cfg <- system.file("extdata", "config/databases.toml", package = "annovarR")
  x <- get.cfg.value.by.name("avsnp138", cfg, key = "return.col.names", coincident = TRUE, 
    extra.list = list(name = "avsnp138"), rcmd.parse = TRUE)
  expect_that(x, equals("avSNP138"))
  x <- get.cfg.value.by.name("1000g2015aug_all", cfg, key = "return.col.names", 
    coincident = TRUE, extra.list = list(name = "1000g2015aug_all"), rcmd.parse = TRUE)
  expect_that(x, equals("1000g2015aug_all"))
})
