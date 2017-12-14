test_that("sqlite.auto.build", {
  i <- "hg19_avsnp147"
  database <- system.file("extdata", sprintf("demo/%s.txt", i), package = "annovarR")
  file.copy(database, sprintf("%s/%s.txt", tempdir(), i))
  x <- sqlite.auto.build("avsnp147", "hg19", database.dir = tempdir(), verbose = FALSE)
  expect_that(x, equals(TRUE))
  x <- sqlite.auto.index("avsnp147", "hg19", database.dir = tempdir(), verbose = FALSE, 
    index = "chr_start_index2")
  expect_that(x, equals(TRUE))
  dbname <- sprintf("%s/%s.sqlite", tempdir(), i)
  x <- sqlite.tb.indexes(list(dbname = dbname, table.name = "hg19_avsnp147"))
  expect_that("chr_start_index" %in% x$name, equals(TRUE))
  expect_that("chr_start_index2" %in% x$name, equals(TRUE))
  unlink(sprintf("%s/%s.txt", tempdir(), i))
  unlink(sprintf("%s/%s.sqlite", tempdir(), i))
})

test_that("BioConductor", {
  gene <- c("TP53", "NSD2")
  x <- annotation(dat = gene, anno.name = "bioc_gene2alias")
  expect_that(is.data.table(x), equals(TRUE))
  expect_that(colnames(x)[1], equals("SYMBOL"))
  expect_that(colnames(x)[2], equals("ALIAS"))
})
