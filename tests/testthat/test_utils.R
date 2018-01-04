
test_that("select.dat.full.match", {
  database <- system.file("extdata", "demo/hg19_cosmic81.txt", package = "annovarR")
  sqlite.db <- sprintf("%s/hg19_cosmic81.sqlite", tempdir())
  txt.db <- sprintf("%s/hg19_cosmic81.txt", tempdir())
  file.copy(database, txt.db)
  sqlite.build(database, list(dbname = sqlite.db, table.name = "hg19_cosmic81"))
  database <- dbConnect(RSQLite::SQLite(), sqlite.db)
  x <- select.dat.full.match(database, "hg19_cosmic81", "V1", list("1"))
  x <- as.data.frame(x)
  expect_that(colnames(x), equals(paste0("V", 1:6)))
  expect_that(x[1, 2], equals(13496008))
  dbDisconnect(database)
  
  sqlite.db <- normalizePath(sqlite.db, "/")
  txt.db <- normalizePath(txt.db, "/")
  file.remove(sqlite.db)
  file.remove(txt.db)
  
  # MySQL service
  if (mysqlHasDefault()) {
    database <- dbConnect(RMySQL::MySQL(), dbname = "annovarr")
    database <- system.file("extdata", "demo/hg19_cosmic81.txt", package = "annovarR")
    y <- system.time(x <- select.dat.full.match(database, "disease", "symbol", 
      list(symbol = rep("AML", 10000)), verbose = FALSE, db.type = "mysql"))
  }
})


test_that("get.annotation.func", {
  x <- get.annotation.func("cosmic70")
  expect_that(x, equals("annotation.auto"))
})


test_that("get.cfg.value.by.name", {
  cfg <- system.file("extdata", "config/databases.toml", package = "annovarR")
  x <- get.cfg.value.by.name("avsnp138", cfg, key = "return.col.names", coincident = TRUE, 
    extra.list = list(anno.name = "avsnp138"), rcmd.parse = TRUE)
  expect_that(x, equals("avSNP138"))
  x <- get.cfg.value.by.name("1000g2015aug_all", cfg, key = "return.col.names", 
    coincident = TRUE, extra.list = list(anno.name = "1000g2015aug_all"), rcmd.parse = TRUE)
  expect_that(x, equals("1000g2015aug_all"))
})

test_that("msg", {
  x <- info.msg("info.msg", verbose = TRUE)
  expect_that(is.character(x), equals(TRUE))
  x <- print.vb("print.vb", verbose = TRUE)
  expect_that(is.character(x), equals(TRUE))
})
