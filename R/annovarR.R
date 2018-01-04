#' annovarR is an integrated database and tool to annotate genetic variants 
#' from genomic and transcriptome sequencing data.
#' This work mainly based on ANNOVAR and other public annotation databases.
#' 
#' @author
#' Li Jianfeng \url{lee_jianfeng@sjtu.edu.cn}
#' @seealso
#' Useful links:
#'
#' \url{https://github.com/JhuangLab/annovarR} 
#'
#' Report bugs at \url{https://github.com/JhuangLab/annovarR/issues} 
#'
#' @docType package
#' @name annovarR
#' @import stringr DBI data.table futile.logger configr BioInstaller glue
#' @importFrom stringi stri_rand_strings
#' @importFrom utils packageVersion head installed.packages
#' @importFrom RMySQL MySQL dbRemoveTable mysqlHasDefault
#' @importFrom RSQLite SQLite dbRemoveTable
#' @importFrom methods formalArgs
#' @importFrom ngstk split_list split_row_file batch_file
#' @importFrom AnnotationDbi select
#' @importFrom vcfR read.vcfR INFO2df getID getINFO getCHROM getPOS getFILTER getALT getFIX
NULL

.onAttach <- function(libname, pkgname) {
  op <- options()
  # msg <- sprintf('annovarR %s \n', packageVersion('annovarR')) msg <-
  # sprintf('%s Documentation: ?annovarR, example(annovarR) and
  # browseVignettes(\'annovarR\')', msg) packageStartupMessage(msg)
  Sys.setenv(R_TESTS = "")
  invisible()
}

