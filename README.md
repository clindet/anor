# [![Build Status](https://travis-ci.org/JhuangLab/annovarR.svg)](https://travis-ci.org/JhuangLab/annovarR) [![License](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat)](https://en.wikipedia.org/wiki/MIT_License) [![CRAN](http://www.r-pkg.org/badges/version/annovarR)](https://cran.r-project.org/package=annovarR) [![Downloads](http://cranlogs.r-pkg.org/badges/annovarR?color=brightgreen)](http://www.r-pkg.org/pkg/annovarR) [![codecov](https://codecov.io/github/JhuangLab/annovarR/branch/master/graphs/badge.svg)](https://codecov.io/github/JhuangLab/annovarR) 

annovarR package
==============
[annovarR](https://github.com/JhuangLab/annovarR) R package is an open source genomic annotation tool based on [ANNOVAR](http://annovar.openbioinformatics.org/en/latest/) and other public databases. Some of RNA-editing and other RNA-seq releated database be added as the supplement of ANNOVAR.

Feature:

-   Download ANNOVAR databases in R
-   Editable configuration file and extern preprocess script to extend annotation system
-   Support for multiple file and data types both input and reference (e.g. text file, sqlite, MySQL)
-   Support cluster computing, by [pannovar](http://github.com/JhuangLab/pannovar)

## Installation

### CRAN
``` r
#You can install this package directly from CRAN by running (from within R):
install.packages('annovarR')
```

### Github
``` bash
# Install the cutting edge development version from Lab-GitHub:
# Now only Jhuanglab members can get the source
# install.packages("devtools")
devtools::install_github("JhuangLab/annovarR")
```

## Support Summary

-   Native ANNOVAR Databases
-   RNA-editing databases
-   Other databases

## Docker

You can use the annovarR in Docker.

```bash
docker pull bioinstaller/annovarR:develop
docker run -it -v /tmp/db:/tmp/db -v /tmp/input:/tmp/input bioinstaller/annovarR:develop R
```

## How to contribute?

Please fork the [GitHub BioInstaller repository](https://github.com/JhuangLab/annovarR), modify it, and submit a pull request to us. 

## Maintainer

[Jianfeng Li](https://github.com/Miachol)

## License

R package:

[MIT](https://en.wikipedia.org/wiki/MIT_License)

Related Other Resources:

[Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License](https://creativecommons.org/licenses/by-nc-nd/4.0/)

