bed.file <- system.file("extdata", "demo/example.bed", package = "annovarR")
bed.sqlite <- sprintf("%s/%s.sqlite", tempdir(), basename(bed.file))
connect.params <- list(dbname = bed.sqlite, table.name = "bed")
sqlite.build(bed.file, connect.params)
refGene.file <- system.file("extdata", "demo/hg19_refGene.txt", package = "annovarR")
sqlite.db.name <- str_replace(basename(refGene.file), ".txt$", ".sqlite")
refGene.sqlite <- sprintf("%s/%s", tempdir(), sqlite.db.name)
connect.params <- list(dbname = refGene.sqlite, table.name = "hg19_refGene")

sqlite.build(refGene.file, connect.params)

test_that("select.dat.region.match.txt", {
  # only select matched rows
  V1 <- c("chr10", "chr1", "chr2", "chr10")
  V2 <- c("100188904", "100185955", "123456", "104590288")
  V3 <- c("100188904", "100185955", "123456", "104597290")
  params <- list(V1 = V1, V2 = V2, V3 = V3)
  x <- select.dat.region.match(bed.file, "bed", "V1", "V2", "V3", params = params, 
    db.type = "txt")
  x <- as.data.frame(x)
  expect_that(nrow(x), equals(3))
  expect_that(x[1, 1], equals("chr10"))
  expect_that(x[2, 1], equals("chr10"))
  expect_that(x[1, 4], equals("HPS1"))
  expect_that(x[2, 4], equals("HPS1"))
  expect_that(x[3, 4], equals("CYP17A1"))
})

test_that("annotation.cscd", {
  chr <- c("chr7", "chrX", "chr1", "chr10")
  start <- c("18631138", "123668609", "10020", "1234")
  end <- c("18669104", "123668736", "10020", "4567")
  dat <- data.table(chr = chr, start = start, end = end)
  database.dir <- system.file("extdata", "demo", package = "annovarR")
  x <- annotation(dat, "cscd_cancer_circrna", database.dir = database.dir, db.type = "txt")
  x <- as.data.frame(x)
  expect_that(nrow(x), equals(4))
  expect_that(x[1, 2], equals("cancer//cancer"))
  expect_that(x[2, 2], equals("cancer"))
  expect_that(is.na(x[3, 1]), equals(TRUE))
  expect_that(is.na(x[4, 1]), equals(TRUE))
  
})


test_that("select.dat.region.match.sqlite", {
  V1 <- c("chr10", "chr1")
  V2 <- c("100188904", "100185955")
  V3 <- c("100188904", "100185955")
  params <- list(V1 = V1, V2 = V2, V3 = V3)
  con <- dbConnect(SQLite(), bed.sqlite)
  x <- select.dat.region.match(con, "bed", "V1", "V2", "V3", params = params, db.type = "sqlite", 
    verbose = FALSE)
  dbDisconnect(con)
  x <- as.data.frame(x)
  expect_that(nrow(x), equals(2))
  expect_that(x[1, 1], equals("chr10"))
  expect_that(x[2, 1], equals("chr10"))
  expect_that(x[1, 4], equals("HPS1"))
  expect_that(x[2, 4], equals("HPS1"))
  
})

test_that("annotation.region.match", {
  chr <- c("chr10", "chr1")
  start <- c("100188904", "100185955")
  end <- c("100188904", "100185955")
  dat <- data.table(chr = chr, start = start, end = end)
  x <- annotation.region.match(dat = dat, database.dir = tempdir(), dbname.fixed = bed.sqlite, 
    table.name.fixed = "bed", db.type = "sqlite", format.dat.fun = format.cols.plus.chr, 
    format.db.tb.fun = format.db.region.tb)
  x <- as.data.frame(x)
  expect_that(x[1, 1], equals("HPS1"))
  expect_that(is.na(x[2, 1]), equals(TRUE))
})

test_that("annotation.region.match:refgene", {
  chr <- c("11", "11", "14")
  start <- c("89057522", "89224732", "52471419")
  end <- c("89057522", "89224732", "52471419")
  dat <- data.table(chr = chr, start = start, end = end)
  x <- annotation.region.match(dat = dat, dbname.fixed = refGene.file, table.name.fixed = "hg19_refGene", 
    db.col.order = c(3, 5, 6), full.matched.col = "chr", inferior.col = "start", 
    format.dat.fun = format.cols.plus.chr, superior.col = "end", return.col.index = 13, 
    return.col.names = "Gene", verbose = FALSE, db.type = "txt")
  x <- as.data.frame(x)
  expect_that(x[1, 1], equals("NOX4"))
  expect_that(x[2, 1], equals("NOX4"))
  expect_that(x[3, 1], equals("C14orf166"))
  x <- annotation.region.match(dat = dat, dbname.fixed = refGene.sqlite, table.name.fixed = "hg19_refGene", 
    db.col.order = c(3, 5, 6), full.matched.col = "chr", inferior.col = "start", 
    format.dat.fun = format.cols.plus.chr, superior.col = "end", return.col.index = 13, 
    return.col.names = "Gene", verbose = FALSE, db.type = "sqlite")
  x <- as.data.frame(x)
  expect_that(x[1, 1], equals("NOX4"))
  expect_that(x[2, 1], equals("NOX4"))
  expect_that(x[3, 1], equals("C14orf166"))
})

test_that("auto:refgene", {
  chr <- c("11", "11", "14")
  start <- c("89057522", "89224732", "52471419")
  end <- c("89057522", "89224732", "52471419")
  dat <- data.table(chr = chr, start = start, end = end)
  x <- annotation(anno.name = "ucsc_refgene", dat = dat, database.dir = tempdir(), 
    db.type = "sqlite", verbose = FALSE)
  expect_that(is.data.table(x), equals(TRUE))
  expect_that(nrow(x), equals(3))
  expect_that(colnames(x)[1], equals("Transcript"))
})


file.remove(bed.sqlite)
file.remove(refGene.sqlite)
