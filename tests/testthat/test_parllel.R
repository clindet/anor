database.dir <- tempdir()
for (i in c("hg19_avsnp147", "hg19_avsnp147.common", "hg19_cosmic81", "hg19_ALL.sites.2015_08", 
  "hg19_RADAR2", "hg19_DARNED", "hg19_normal2016sih_wes_ball", "hg19_normal2016sih_wes_nkt", 
  "hg19_normal2016sih_wes_tall", "hg19_normal2016sih_wgs_nkt", "hg19_normal2016sih_wgs_dlbcl", 
  "hg19_clinvar_20170130", "hg19_intervar_20170202", "hg19_REDIportal")) {
  database <- system.file("extdata", sprintf("demo/%s.txt", i), package = "annovarR")
  sqlite.db <- sprintf("%s/%s.sqlite", tempdir(), i)
  file.copy(database, sprintf("%s/%s.txt", tempdir(), i))
  sqlite.build(database, sqlite.connect.params = list(sqlite.path = sqlite.db, 
    table.name = sprintf("%s", i)))
}

test_that("parAnnotation", {
  # chr <- c('chr1', 'chr2', 'chr1', 'chr12', 'chr2', 'chr12') start <- c('10020',
  # '10020', '10020', '11139001', '50850617', '11139002') end <- c('10020',
  # '10020', '10020', '11139001', '50850617', '50850617') ref <- c('A', 'A', 'A',
  # 'C', 'G', 'T') alt <- c('-', '-', '-', 'T', 'A', '-') dat <- data.table(chr =
  # chr, start = start, end = end, ref = ref, alt = alt) row.cl <- makeCluster(2) x
  # <- parAnnotation(dat = dat, anno.names = c('avsnp147', 'cosmic81'),
  # database.dir = database.dir, row.cl = row.cl, col.cl.num = 2)
  # stopCluster(row.cl) x <- as.data.frame(x) expect_that(colnames(x)[1],
  # equals('avSNP147')) expect_that(colnames(x)[2], equals('COSMIC_81')) x[, 1] <-
  # as.character(x[, 1]) x[, 2] <- as.character(x[, 2]) expect_that(x[1, 1],
  # equals('rs775809821')) expect_that(is.na(x[2, 1]), equals(TRUE))
  # expect_that(x[3, 1], equals('rs775809821')) expect_that(x[4, 2],
  # equals('ID=COSM4732228;OCCURENCE=1(large_intestine)')) expect_that(x[5, 2],
  # equals('ID=COSM383974,COSM5973818,COSM383973,COSM5973817;OCCURENCE=1(upper_aerodigestive_tract),1(lung)'))
  # expect_that(is.na(x[6, 2]), equals(TRUE)) row.cl <- makeCluster(2) x <-
  # parAnnotation(dat = dat, anno.names = c('avsnp147', 'cosmic81'), database.dir =
  # database.dir, db.type = 'txt', row.cl = row.cl) stopCluster(row.cl) x <-
  # as.data.frame(x) expect_that(colnames(x)[1], equals('avSNP147'))
  # expect_that(colnames(x)[2], equals('COSMIC_81')) x[, 1] <- as.character(x[, 1])
  # x[, 2] <- as.character(x[, 2]) expect_that(is.na(x[6, 2]), equals(TRUE))
  # expect_that(x[1, 1], equals('rs775809821')) expect_that(is.na(x[2, 1]),
  # equals(TRUE)) expect_that(x[3, 1], equals('rs775809821')) expect_that(x[4, 2],
  # equals('ID=COSM4732228;OCCURENCE=1(large_intestine)')) expect_that(x[5, 2],
  # equals('ID=COSM383974,COSM5973818,COSM383973,COSM5973817;OCCURENCE=1(upper_aerodigestive_tract),1(lung)'))
  # expect_that(is.na(x[6, 2]), equals(TRUE))
})

for (i in c("hg19_avsnp147", "hg19_avsnp147.common", "hg19_cosmic81", "hg19_ALL.sites.2015_08", 
  "hg19_RADAR2", "hg19_DARNED", "hg19_normal2016sih_wes_ball", "hg19_normal2016sih_wes_nkt", 
  "hg19_normal2016sih_wes_tall", "hg19_normal2016sih_wgs_nkt", "hg19_normal2016sih_wgs_dlbcl", 
  "hg19_clinvar_20170130", "hg19_intervar_20170202", "hg19_REDIportal")) {
  sqlite.db <- sprintf("%s/%s.sqlite", tempdir(), i)
  txt.db <- sprintf("%s/%s.txt", tempdir(), i)
  sqlite.db <- normalizePath(sqlite.db, "/")
  txt.db <- normalizePath(txt.db, "/")
  file.remove(sqlite.db)
  file.remove(txt.db)
}
