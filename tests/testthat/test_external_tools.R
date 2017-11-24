test_that("annovar", {
down.dbname <- 'refGene'
x <- annovar("perl", down.dbname = "avsnp147", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)
x <- annovar("perl", cmd.used = "script1.gene.based", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)
x <- annovar("perl", cmd.used = "script1.region.based", dbtype = "cytoBand", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)
x <- annovar("perl", cmd.used = "script1.filter.based", dbtype = "avsnp147", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)
x <- annovar("perl", cmd.used = "script2", dbtype = "refGene,cytoBand,avsnp147", 
             input.file = "example.avinput", annovar.dir = "~/tmp/annovar.dir", debug = TRUE)
})
