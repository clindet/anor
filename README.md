# [![Build Status](https://travis-ci.org/JhuangLab/annovarR.svg)](https://travis-ci.org/JhuangLab/annovarR) [![License](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat)](https://en.wikipedia.org/wiki/MIT_License) [![codecov](https://codecov.io/github/JhuangLab/annovarR/branch/master/graphs/badge.svg)](https://codecov.io/github/JhuangLab/annovarR) 

annovarR package
==============
[annovarR](https://github.com/JhuangLab/annovarR) is a novel integrated open source tool to annotate genetic variants data based on [ANNOVAR](http://annovar.openbioinformatics.org/en/latest/) and other public annotation databases, such as [varcards](http://varcards.biols.ac.cn/), [REDIportal](http://srv00.recas.ba.infn.it/atlas/), .etc. 

The main development motivation of annovarR is to increase [ANNOVAR](http://annovar.openbioinformatics.org/en/latest/) the supported database and facilitate the variants annotation work in R.

In addition, to provide more transcription levels of variant database resources, we collected total 1285 cases public B-progenitor acute lymphoblastic leukemia (B-ALL) transcriptome data from five different published datasets and built a novel large-scale transcript level sequencing variant database. [The Genome Analysis Toolkit (GATK)](https://software.broadinstitute.org/gatk/), [VarScan2](http://massgenomics.org/varscan) and [LoFreq](http://csb5.github.io/lofreq/) be used to call variants from the RNA-seq data (Database called BRVar). This work can help us to screen candidate systematic sequencing bias and evaluate variant calling trait from RNA-seq.

```r
# Download BRVar database
# Firstly, you need to apply a licence code contact Jianfeng: lee_jianfeng@sjtu.edu.cn
library(annovarR)
download.database("db_annovar_brvar", "/path/annovar.dir", extra.list = list(licence = "licence_code"))
```

Feature:

-   One-click download ANNOVAR and other public databases in R based on our previous developed [BioInstaller](https://github.com/JhuangLab/BioInstaller) R package which can be used to download/install bioinformatics tools, dependences and databases in R relatively easily.
-   The editable configuration file and external acceptable function to control the annotation behavior.
-   Support for multiple data types both in input data and reference database(e.g. text file, SQLite, MySQL).
-   Provide support for parallel computing, by additional R package [pannovar](http://github.com/JhuangLab/pannovar) that can be used to parallelly finish annotation work using external annotation tool.
-   Annotate 5000000 unique lines genetic variants from two database (cosmic81 and avsnp147 be tested) in single thread mode noly need 20 seconds.
-   Easily to convert very large files to sqlite database in R.
-   Access the database meta information in R conveniently.

## Installation

### CRAN
``` r
#You can install this package directly from CRAN in the next release version (from within R):
install.packages('annovarR')
```

### Github
``` bash
# install.packages("devtools")
devtools::install_github("JhuangLab/annovarR")
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

# Annotation variants from avsnp147 database
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

