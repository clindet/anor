test_that("return.empty.col", {
  dat.list <- list(a = 1:100, b = 1:100)
  tb.colnames <- c("a", "b")
  return.col.index <- 1
  return.col.names <- "test_a"
  return.col.names.profix <- "profix_"
  x <- do.call(return.empty.col, list(dat.list, tb.colnames, return.col.index, 
    return.col.names, return.col.names.profix))
  expect_that(colnames(x), equals("profix_test_a"))
  return.col.names <- ""
  x <- do.call(return.empty.col, list(dat.list, tb.colnames, return.col.index, 
    return.col.names, return.col.names.profix))
  expect_that(colnames(x), equals("profix_a"))
})
