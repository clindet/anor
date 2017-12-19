# [![Build Status](https://travis-ci.org/JhuangLab/annovarR.svg)](https://travis-ci.org/JhuangLab/annovarR) [![License](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat)](https://en.wikipedia.org/wiki/MIT_License) [![codecov](https://codecov.io/github/JhuangLab/annovarR/branch/master/graphs/badge.svg)](https://codecov.io/github/JhuangLab/annovarR) 

annovarR package
==============
[annovarR](https://github.com/JhuangLab/annovarR) is an integrated open source tool to annotate genetic variants data based on [ANNOVAR](http://annovar.openbioinformatics.org/en/latest/) and other public annotation databases, such as [varcards](http://varcards.biols.ac.cn/), [REDIportal](http://srv00.recas.ba.infn.it/atlas/), .etc. 

The main development motivation of annovarR is to increase the supported database and facilitate the variants annotation work. There are already too many tools and databases available and the usage is quite different. annovarR will integrate these annotation tools and get the merged annotation result in R.

annovarR will not only provide annotation functions (both internal and external) but also established an annotation database pool including published and community contributed.

In addition, to provide more transcription levels of variant database resources, we collected total 1285 cases public B-progenitor acute lymphoblastic leukemia (B-ALL) transcriptome data from five different published datasets and built a novel large-scale transcript level sequencing variant database. [The Genome Analysis Toolkit (GATK)](https://software.broadinstitute.org/gatk/), [VarScan2](http://massgenomics.org/varscan) and [LoFreq](http://csb5.github.io/lofreq/) be used to call variants from the RNA-seq data (Database called BRVar). This work can help us to screen candidate systematic sequencing bias and evaluate variant calling trait from RNA-seq.

```r
# Download BRVar database
# Firstly, you need to apply a licence code contact Jianfeng: lee_jianfeng@sjtu.edu.cn
library(annovarR)
download.database("db_annovar_brvar", "/path/annovar.dir",  license = "licence_code")
```

## Requirements

annovarR annotation system:

- R >= 3.3.0
- [SQLite](http://www.sqlite.org/download.html)

ANNOVAR annotation system:

- [perl](http://strawberryperl.com/)
- [ANNOVAR](http://annovar.openbioinformatics.org/en/latest/)

## Installation

### Core softwares

``` r
# CRAN to install annovarR (The R package BioInstaller will be installed)
setRepositories(ind=1:2)
install.packages('annovarR')

# Github to install annovarR (The R package BioInstaller will be installed)
# install.packages("devtools")
devtools::install_github("JhuangLab/annovarR")

# Use BioInstaller to install ANNOVAR easily in R
library(BioInstaller)
install.bioinfo('annovar', '/path/annovar.dir')
```

### Annotation Database

``` r
# Use download.database to download databases supported by annovarR and ANNOVAR
library(annovarR)
download.database('db_annovar_refgene', database.dir = "/path/database.dir/humandb", buildver = "hg19")
download.database('db_annovar_cytoband', database.dir = "/path/database.dir/humandb", buildver = "hg19")
download.database('db_annovar_avsnp147', database.dir = "/path/database.dir/humandb", buildver = "hg19")
# Or
download.database(c("db_annovar_refgene", "db_annovar_cytoband", "db_annovar_avsnp147"), 
  database.dir = "/path/database.dir/humandb", buildver = "hg19")

# All annovarR supported big annotation database required SQLite format
download.database('db_annovar_avsnp147_sqlite', database.dir = "/path/database.dir/humandb", buildver = "hg19")
```

## Support Summary

-   [ANNOVAR databases](http://annovar.openbioinformatics.org/en/latest/)
-   1285 cases B-ALL RNA-seq variants 
-   Public RNA-editing databases
-   Other public database

## Basic Usage

```r
# Get all annovarR supported annotation name
get.annotation.names()

# Get annotation name needed download.name and 
# you can use download.database to download database using the download.name.
download.name <- get.download.name('avsnp147')

# Show download.name avaliable all versions database
download.database(download.name = download.name, show.all.versions = TRUE)
# Download database in annotation database directory
# Buildver default is hg19
download.database(download.name = download.name, version = "avsnp147", buildver = "hg19", 
  database.dir = "/path/database.dir")

# Annotation variants from avsnp147 database use annovarR
library(data.table)
database.dir <- "/path/database.dir"
chr <- c("chr1", "chr2", "chr1")
start <- c("10020", "10020", "10020")
end <- c("10020", "10020", "10020")
ref <- c("A", "A", "A")
alt <- c("-", "-", "-")
database.dir <- tempdir()
dat <- data.table(chr = chr, start = start, end = end, ref = ref, alt = alt)
x <- annotation(dat = dat, anno.name = "avsnp147", database.dir = database.dir)

# Annotation multiple database
x <- annotation.merge(dat = dat, anno.names = c("cosmic81", "avsnp147"), database.dir = database.dir)

# Database configuration file
database.cfg <- system.file('extdata', 'config/databases.toml', package = "annovarR")

# Get anno.name needed input cols
get.annotation.needcols('avsnp147')

# Annotation avinput format R data and file use ANNOVAR
annovar.dir <- "/opt/bin/annovar"
database.dir <- "/opt/bin/annovar/humandb"
chr = "chr1"
start = "123"
end = "123"
ref = "A"
alt = "C"
dat <- data.table(chr, start, end, ref, alt)
tmpfn <- tempfile()
write.table(dat, fn, row.names = FALSE, quote = FALSE, sep = "\t", col.names = FALSE)
x <- annotation(dat, "perl_annovar_refGene", annovar.dir = "/opt/bin/annovar", 
             database.dir = database.dir)
x <- annotation(input.file = tmpfn, "perl_annovar_refGene", annovar.dir = "/opt/bin/annovar", 
             database.dir = database.dir)

# Annotation avinput format R data use annovarR and ANNOVAR
# It will return a list contatin two data.table object that 
# one is annovarR annotation system and the another is ANNOVAR output 
x <-annotation.merge(dat = dat, anno.names = c('avsnp147', 'perl_annovar_refGene'), 
  annovar.dir = annovar.dir, database.dir = database.dir)
x <- annotation.merge(dat = dat, anno.names = c('avsnp147', '1000g2015aug_all', 
  'perl_annovar_refGene', 'perl_annovar_ensGene'), annovar.dir = annovar.dir, database.dir = database.dir)
# If use perl_annovar_merge as the anno.name, you can use annovar.anno.names to 
# run all original ANNOVAR supported annotation names, see http://annovar.openbioinformatics.org/en/latest/user-guide/download/
x <- annotation.merge(dat = dat, anno.names = c('avsnp147', '1000g2015aug_all', 
  'perl_annovar_merge'), annovar.anno.names = c('refGene', 'ensGene'), annovar.dir = annovar.dir, database.dir = database.dir)

# Annotation VCF file use ANNOVAR

x <- annotation(anno.name = "perl_annovar_ensGene", input.file = "/tmp/test.vcf",
             annovar.dir = annovar.dir, database.dir = "{{annovar.dir}}/humandb", 
             out = tempfile(), vcfinput = TRUE)

# Annotation data use BioConductor database
# The example below will use the org.Hs.eg.db to get the alias of TP53 and NSD2
# It is more simple than the previous annotation API
gene <- c("TP53", "NSD2")
x <- annotation(dat = gene, anno.name = "bioc_gene2alias")

# Do same things use AnnotationDbi
library(org.Hs.eg.db)
library(AnnotationDbi)
select(keys = gene, keytype = "SYMBOL", columns = "ALIAS")

```

## Docker

You can use the annovarR in Docker.

```bash
docker pull bioinstaller/annovarr:develop
docker run -it -v /tmp/db:/tmp/db -v /tmp/input:/tmp/input bioinstaller/annovarr:develop R
```

## How to contribute?

Please fork the [GitHub annovarR repository](https://github.com/JhuangLab/annovarR), modify it, and submit a pull request to us. 

## Maintainer

[Jianfeng Li](https://github.com/Miachol)

## License

R package:

[MIT](https://en.wikipedia.org/wiki/MIT_License)

Related Other Resources:

[Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License](https://creativecommons.org/licenses/by-nc-nd/4.0/)

