test_that("annovar", {
down.dbname <- 'refGene'
x <- annovar("perl", down.dbname = "avsnp147", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)
x <- annovar("perl", cmd.used = "script1.gene.based", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)
x <- annovar("perl", cmd.used = "script1.region.based", anno.names = "cytoBand", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)
x <- annovar("perl", cmd.used = "script1.filter.based", anno.names = "avsnp147", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)
x <- annovar("perl", cmd.used = "script2", anno.names = c("refGene", "cytoBand", "avsnp147"), 
             input.file = "example.avinput", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)

x <- annovar("perl", cmd.used = "script3", format = "vcf4old", 
             input.file = "example.vcf", convert.out = "example.avinput", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)
})
