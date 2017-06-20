database.dir <- tempdir()
for (i in c("hg19_avsnp147", "hg19_cosmic81")) {
  database <- system.file("extdata", sprintf("demo/%s.txt", i), package = "annovarR")
  sqlite.db <- sprintf("%s/%s.sqlite", tempdir(), i)
  file.copy(database, sprintf("%s/%s.txt", tempdir(), i))
  sqlite.build(database, sqlite.connect.params = list(dbname = sqlite.db, table.name = sprintf("%s", 
    i)))
}

test_that("big file", {
  if ("annovarR" %in% .packages(all.available = T)) {
    #chr <- c("chr1", "chr2", "chr1", "chr12", "chr2", "chr12")
    #start <- c("10020", "10020", "10020", "11139001", "50850617", "11139002")
    #end <- c("10020", "10020", "10020", "11139001", "50850617", "50850617")
    #ref <- c("A", "A", "A", "C", "G", "T")
    #alt <- c("-", "-", "-", "T", "A", "-")
    #dat <- data.table(chr = rep(chr, 1e+04), start = rep(start, 1e+04), end = rep(end, 
    #  1e+04), ref = rep(ref, 1e+04), alt = rep(alt, 1e+04))
    #x <- system.time(result <- parAnnotation.big.file(dat = dat, anno.names = c("avsnp147", "cosmic81"), 
    #  database.dir = database.dir, db.type = "txt"))
    #dat.file <- tempfile()
    #fwrite(dat, dat.file, sep = "\t")
    #x <- system.time(result <- parAnnotation.big.file(filename = dat.file, anno.names = c("avsnp147"), 
    #  database.dir = database.dir, db.type = "txt"))
  }
})

for (i in c("hg19_avsnp147", "hg19_cosmic81")) {
  sqlite.db <- sprintf("%s/%s.sqlite", tempdir(), i)
  txt.db <- sprintf("%s/%s.txt", tempdir(), i)
  sqlite.db <- normalizePath(sqlite.db, "/")
  txt.db <- normalizePath(txt.db, "/")
  file.remove(sqlite.db)
  file.remove(txt.db)
}
