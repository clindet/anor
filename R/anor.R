#' The 'anor' package provides R functions as well as database resources which offer an
#' integrated framework to annotate genetic variants from genome and transcriptome data.
#' The wrapper functions of 'anor' unified the interface of many published annotation tools,
#' such as 'VEP' (\url{http://asia.ensembl.org/info/docs/tools/vep/index.html}),
#' 'ANNOVAR' (\url{http://annovar.openbioinformatics.org/}), 'vcfanno' (\url{https://github.com/brentp/vcfanno})
#' and 'AnnotationDbi' (\url{http://www.bioconductor.org/packages/release/bioc/html/AnnotationDbi.html}).
#' It also simplified the use of some of the external annotation tools in R.
#' Besides, massive published genetic variants annotation databases were integrated into 'anor'.
#' For example, 'anor' provides a newly RNA-seq allele frequency database, BRVar,
#' which built from total 1,285 cases public B-progenitor acute lymphoblastic leukemia (B-ALL) transcriptome data.
#'
#' @author
#' Li Jianfeng \url{lee_jianfeng@sjtu.edu.cn}
#' @seealso
#' Useful links:
#'
#' \url{https://github.com/JhuangLab/anor}
#'
#' Report bugs at \url{https://github.com/JhuangLab/anor/issues}
#'
#' @docType package
#' @name anor
#' @import stringr DBI data.table futile.logger configr BioInstaller glue liteq
#' @importFrom stringi stri_rand_strings
#' @importFrom utils packageVersion head installed.packages compareVersion install.packages
#' @importFrom RMySQL MySQL dbRemoveTable mysqlHasDefault
#' @importFrom RSQLite SQLite dbRemoveTable
#' @importFrom methods formalArgs
#' @importFrom ngstk split_list split_row_file batch_file
#' @importFrom AnnotationDbi select
#' @importFrom vcfR read.vcfR INFO2df getID getINFO getCHROM getPOS getFILTER getALT getFIX
#' @importFrom devtools install_github
#'
NULL

.onAttach <- function(libname, pkgname) {
  op <- options()
  # msg <- sprintf('anor %s \n', packageVersion('anor')) msg <-
  # sprintf('%s Documentation: ?anor, example(anor) and
  # browseVignettes(\'anor\')', msg) packageStartupMessage(msg)
  Sys.setenv(R_TESTS = "")
  invisible()
}
