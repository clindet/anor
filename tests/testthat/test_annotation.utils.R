test_that("return.empty.col", {
  dat.list <- list(a = 1:100, b = 1:100)
  tb.colnames <- c("a", "b")
  return_col_index <- 1
  return_col_names <- "test_a"
  return_col_names_profix <- "profix_"
  x <- do.call(return.empty.col, list(dat.list, tb.colnames, return_col_index, 
    return_col_names, return_col_names_profix))
  expect_that(colnames(x), equals("profix_test_a"))
  return_col_names <- ""
  x <- do.call(return.empty.col, list(dat.list, tb.colnames, return_col_index, 
    return_col_names, return_col_names_profix))
  expect_that(colnames(x), equals("profix_a"))
})
