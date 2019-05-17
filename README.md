# annovarR <img src="https://github.com/JhuangLab/annovarR/raw/master/man/figures/logo.png" align="right" />

[![Build
Status](https://img.shields.io/circleci/project/github/JhuangLab/annovarR/master.svg)](https://circleci.com/gh/JhuangLab/annovarR/tree/master)
[![CRAN](http://www.r-pkg.org/badges/version/annovarR)](https://cran.r-project.org/package=annovarR) [![Downloads](http://cranlogs.r-pkg.org/badges/annovarR?color=brightgreen)](http://www.r-pkg.org/pkg/annovarR) [![License](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat)](https://en.wikipedia.org/wiki/MIT_License) [![codecov](https://codecov.io/github/JhuangLab/annovarR/branch/master/graphs/badge.svg)](https://codecov.io/github/JhuangLab/annovarR) 

## Introduction

The [annovarR](https://life2cloud.com/tools/annovarR) package provides R functions as well as database resources which offer an integrated framework to annotate genetic variants from genome and transcriptome data. The wrapper functions of annovarR unified the interface of many published annotation tools, such as [VEP](http://asia.ensembl.org/info/docs/tools/vep/index.html), [ANNOVAR](http://annovar.openbioinformatics.org/), [vcfanno](https://github.com/brentp/vcfanno) and [AnnotationDbi](http://www.bioconductor.org/packages/release/bioc/html/AnnotationDbi.html). 

It also simplified the use of some of the external annotation tools in R. Besides, massive published genetic variants annotation databases were integrated into annovarR.

The main development motivation of annovarR is to increase the supported database and facilitate the variants annotation work. There are already too many tools and databases available and the usage is quite different. annovarR will integrate these annotation tools and get the merged annotation result in R.

<img src="https://github.com/JhuangLab/annovarR/raw/master/man/figures/annovarR_package-1.jpg" align="center" />

annovarR will not only provides annotation functions (both internal and external) but also hope to establish a shared annotation database resources. In fact, we have made some efforts in the relevant direction that massive scattered databases can easily to be download via using [BioInstaller](https://github.com/JhuangLab/BioInstaller) R package.

Besides, we collected total 1,285 cases public B-progenitor acute lymphoblastic leukemia (B-ALL) transcriptome data from five different published datasets and built a novel large-scale transcript level sequencing variant database. [The Genome Analysis Toolkit (GATK)](https://software.broadinstitute.org/gatk/), [VarScan2](http://massgenomics.org/varscan) and [LoFreq](http://csb5.github.io/lofreq/) be used to call variants from the RNA-seq data (Database called BRVar). This work can help us to screen candidate systematic sequencing bias and evaluate variant calling trait from B-ALL RNA-seq.

<img src="https://github.com/JhuangLab/annovarR/raw/master/man/figures/BRVar_databases-1.jpg" align="center" />

If you want to download the 1,285 RNA-seq variants frequency database, you need click [here](http://bioinfo.rjh.com.cn/labs/jhuang/contact.php) and send us a short application message (Whether it is for commercial use?). If the application is applied, we will reply you within 24 hours with a download required license code.

```r
# Download BRVar database
# You must input the applied license code
library(annovarR)
download.database("db_annovar_brvar", "/path/annovar.dir",  license = "licence_code")
```

## Requirements

annovarR annotation system:

- [R](https://cran.r-project.org/) >= 3.3.0
- [perl](http://strawberryperl.com/)
- [SQLite](http://www.sqlite.org/download.html)
- [AnnotationDbi](http://www.bioconductor.org/packages/release/bioc/html/AnnotationDbi.html)

ANNOVAR annotation system:

- [ANNOVAR](http://annovar.openbioinformatics.org/en/latest/)

VEP annotation system

- [perl](http://strawberryperl.com/)
- [VEP](http://asia.ensembl.org/info/docs/tools/vep/index.html)

vcfanno annotationo system

- [vcfanno](https://github.com/brentp/vcfanno)

## Installation

### Core softwares

``` r
# CRAN to install annovarR (The R package BioInstaller will be installed)
setRepositories(ind=1:2)
install.packages('annovarR')

# Github to install annovarR (The R package BioInstaller will be installed)
# install.packages("devtools")
devtools::install_github("JhuangLab/annovarR")

# Set needed directory
annovar.dir <- "/opt/bin/annovar"
database.dir <- "/opt/bin/annovar/humandb"
vcfanno.dir <- "/opt/bin/vcfanno.dir"

# Use BioInstaller to install ANNOVAR easily in R
library(BioInstaller)
install.bioinfo('annovar', annovar.dir)

# Use BioInstaller to install vcfanno easily in R
# Support linux and mac
install.bioinfo('vcfanno', vcfanno.dir)
```

### Annotation Database

``` r
# Use download.database to download databases supported by annovarR and ANNOVAR
# Some of examples as shown as below
library(annovarR)
download.database('db_annovar_refgene', database.dir = database.dir, buildver = "hg19")
download.database('db_ucsc_cytoband', database.dir = database.dir, buildver = "hg19")
download.database('db_annovar_avsnp147', database.dir = database.dir, buildver = "hg19")
# Or
download.database(c("db_annovar_refgene", "db_ucsc_cytoband"), 
  database.dir = database.dir, buildver = "hg19")

# All annovarR supported big annotation database required SQLite format
download.database('db_annovar_avsnp147_sqlite', database.dir = database.dir, buildver = "hg19")
```

## Support Summary

-   [ANNOVAR databases](http://annovar.openbioinformatics.org/en/latest/)
-   1285 cases B-ALL RNA-seq variants 
-   Public RNA-editing databases
-   Other public database

## Basic Usage

```r
# View the vignettes in annovarR package
browseVignettes("annovarR")

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
  database.dir = database.dir)

# Annotate variants from avsnp147 database use annovarR
library(data.table)
chr <- c("chr1", "chr2", "chr1")
start <- c("10020", "10020", "10020")
end <- c("10020", "10020", "10020")
ref <- c("A", "A", "A")
alt <- c("-", "-", "-")
dat <- data.table(chr = chr, start = start, end = end, ref = ref, alt = alt)
x <- annotation(dat = dat, anno.name = "avsnp147", database.dir = database.dir)

# Annotate using multiple database
x <- annotation.merge(dat = dat, anno.names = c("cosmic81", "avsnp147"), database.dir = database.dir)

# Database configuration file
database.cfg <- system.file('extdata', 'config/databases.toml', package = "annovarR")

# Get anno.name needed input cols
get.annotation.needcols('avsnp147')

# Annotate avinput format R data and file using ANNOVAR
chr = "chr1"
start = "123"
end = "123"
ref = "A"
alt = "C"
dat <- data.table(chr, start, end, ref, alt)
tmpfn <- tempfile()
write.table(dat, tmpfn, row.names = FALSE, quote = FALSE, sep = "\t", col.names = FALSE)
x <- annotation(dat, anno.name = "perl_annovar_refGene", annovar.dir = annovar.dir, 
             database.dir = database.dir)
x <- annotation(input.file = tmpfn, anno.name = "perl_annovar_refGene", annovar.dir = annovar.dir, 
             database.dir = database.dir)

# Annotate avinput format R data using annovarR and ANNOVAR
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

# Annotate VCF file using ANNOVAR
test.vcf.raw <- system.file("extdata", "demo/example.vcf", package = "annovarR") 
test.vcf <- tempfile()
file.copy(test.vcf.raw, test.vcf)
x <- annotation(anno.name = "perl_annovar_ensGene", input.file = test.vcf,
             annovar.dir = annovar.dir, database.dir = database.dir, 
             out = tempfile(), vcfinput = TRUE)

# Annotate VCF file use VEP
vep(debug = TRUE)
x <- annotation(anno.name = "vep_all", input.file = test.vcf,
             out = tempfile(), debug = TRUE)

# Annotate VCF file use vcfanno
vcfanno(debug = TRUE)
x <- annotation(anno.name = "vcfanno_demo", input.file = test.vcf,
                   out = tempfile(), vcfanno = sprintf("%s/vcfanno_linux64", vcfanno.dir))

# Annotate VCF file use ANNOVAR and vcfanno
x <- annotation.merge(input.file = test.vcf, anno.names = c("vcfanno_demo", "perl_annovar_ensGene"), 
  annovar.dir = annovar.dir, database.dir = database.dir, vcfanno = sprintf("%s/vcfanno_linux64", vcfanno.dir))

# Annotate gene using BioConductor database
# The example below will use the org.Hs.eg.db to get the alias of TP53 and NSD2
# It is more simple than the previous annotation API
gene <- c("TP53", "NSD2")
x <- annotation(dat = gene, anno.name = "bioc_gene2alias")

# Do same things use AnnotationDbi
library(org.Hs.eg.db)
library(AnnotationDbi)
select(keys = gene, keytype = "SYMBOL", columns = "ALIAS")

```

There are still plenty of places to go for this tool. More annotation tools and annotation databases are being integrated. We hope this tool can provide some help for your variants annotation work.

## Shiny App

In the [BoInstaller](https://github.com/JhuangLab/BioInstaller) bioshiny application, we developed several bioshiny plugins, such as  [ANNOVAR](http://annovar.openbioinformatics.org/en/latest/), [vcfanno](https://github.com/brentp/vcfanno), [maftools](https://github.com/PoisonAlien/maftools), [gvmap](https://github.com/ytdai/gvmap), and so on.

```bash
echo 'export BIO_SOFTWARES_DB_ACTIVE="~/.bioshiny/info.yaml" >> ~/.bashrc'
echo 'export BIOSHINY_CONFIG="~/.bioshiny/shiny.config.yaml" >> ~/.bashrc'
. ~/.bashrc

# Start the standalone Shiny application
wget https://raw.githubusercontent.com/openbiox/bioshiny/master/bin/bioshiny_deps_r
wget https://raw.githubusercontent.com/openbiox/bioshiny/master/bin/bioshiny_deps_3rd
wget https://raw.githubusercontent.com/openbiox/bioshiny/master/bin/bioshiny_start
chmod a+x bioshiny_deps_r
chmod a+x bioshiny_deps_3rd
chmod a+x bioshiny_start
./bioshiny_deps_r
./bioshiny_deps_3rd

# Start Shiny application workers
Rscript -e "bioshiny::set_shiny_workers(1)"
./bioshiny_start

# or use yarn
yarn global add bioshiny
bioshiny_deps_r
bioshiny_deps_3rd
Rscript -e "bioshiny::set_shiny_workers(1)"
bioshiny_start

```

## Docker

You can use the annovarR in Docker.

```bash
docker run -it -v /tmp/db:/tmp/db -v /tmp/input:/tmp/input -v /home/user/.annovarR:/home/opencpu/.annovarR -p 80:80 bioinstaller/annovarr
```

## How to contribute?

Please fork the [GitHub annovarR repository](https://github.com/JhuangLab/annovarR), modify it, and submit a pull request to us. 

## Maintainer

[Jianfeng Li](https://github.com/Miachol)

## License

R package:

[MIT](https://en.wikipedia.org/wiki/MIT_License)

Related Other Resources:

Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License

