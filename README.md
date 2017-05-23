# [![Build Status](https://travis-ci.org/Miachol/annovarR.svg)](https://travis-ci.org/Miachol/annovarR) [![License](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat)](https://en.wikipedia.org/wiki/MIT_License) [![CRAN](http://www.r-pkg.org/badges/version/annovarR)](https://cran.r-project.org/package=annovarR) [![Downloads](http://cranlogs.r-pkg.org/badges/annovarR?color=brightgreen)](http://www.r-pkg.org/pkg/annovarR) [![codecov](https://codecov.io/github/Miachol/annovarR/branch/master/graphs/badge.svg)](https://codecov.io/github/Miachol/annovarR) 

annovarR package
==============

[ANNOVAR](http://annovar.openbioinformatics.org/en/latest/) is an efficient software tool to utilize update-to-date information to functionally annotate genetic variants detected from diverse genomes. Based on several databases of ANNOVAR, annovarR can be used to annotate the genetic variants in R.

annovarR can be extend easily using the sqlite or plain text file as the refference database and more databases will be supported in the future using our clinical data.

## Support Summary

**dbSNP**

- avsnp138, avsnp142, avsnp144, avsnp147

**Cosmic**

- cosmic70, cosmic81

**1000 Genome Project**

- 1000g2015aug_all, 1000g2015aug_afr, 1000g2015aug_amr, 1000g2015aug_eas, 1000g2015aug_eur, 1000g2015aug_sas

**Clinical**

- clinvar_20170130 intervar_20170202
