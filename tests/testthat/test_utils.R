database <- system.file("extdata", "demo/hg19_cosmic81.sqlite", package = "annovarR")
test_that("select.dat", {
  database <- dbConnect(RSQLite::SQLite(), database)
  x <- select.dat(database, "hg19_cosmic81", "V1", list("1"))
  expect_that(colnames(x), equals(paste0("V", 1:6)))
  expect_that(x[1, 2], equals(13496008))
})


test_that("get.annotation.func", {
  x <- get.annotation.func("cosmic70")
  expect_that(x, equals("annotation.cosmic"))
})
