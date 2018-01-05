test_that("annovar.auto", {
  chr = "chr1"
  start = "123"
  end = "123"
  ref = "A"
  alt = "C"
  dat <- data.table(chr, start, end, ref, alt)
  x <- annotation(dat, "perl_annovar_refGene", annovar.dir = "/opt/bin/annovar", 
    database.dir = "{annovar.dir}/humandb", debug = TRUE)
  x <- annotation(anno.name = "perl_annovar_ensGene", input.file = "/tmp/test.vcf", 
    annovar.dir = "/opt/bin/annovar/", database.dir = "{annovar.dir}/humandb", 
    out = tempfile(), vcfinput = TRUE, extra.params = " --csvout", debug = TRUE)
  expect_that(chr, equals("chr1"))
})
