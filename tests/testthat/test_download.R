test_that("download.database",{
  demo.cfg <- system.file("extdata", "demo/demo.cfg", package = "annovarR")
  x <- download.database('download_demo', show.all.versions = T, database.cfg = demo.cfg)
  expect_that(x, equals("demo"))
  x <- download.database('download_demo', "demo", buildver = "hg19",  database.dir = sprintf('%s/databases/', tempdir()),
     database.cfg = demo.cfg)
  expect_that(x, equals(TRUE))
  finalfn <- sprintf("%s/databases/hg19_demo.sqlite", tempdir())
  x <- file.exists(finalfn)
  expect_that(x, equals(TRUE))
  x <- file.size(finalfn)
  expect_that(x > 0, equals(TRUE))
})
