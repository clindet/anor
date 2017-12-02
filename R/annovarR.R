#' annovarR is an novel integrated open source tool to annotate genetic variants data 
#' based on ANNOVAR and other public annotation databases, 
#' such as [varcards](http://varcards.biols.ac.cn/), 
#' [REDIportal](http://srv00.recas.ba.infn.it/atlas/), .etc. 
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
#' @import stringr DBI data.table futile.logger configr BioInstaller
#' @importFrom stringi stri_rand_strings
#' @importFrom utils packageVersion head
#' @importFrom RMySQL MySQL dbRemoveTable mysqlHasDefault
#' @importFrom RSQLite SQLite dbRemoveTable
#' @importFrom methods formalArgs
#' @importFrom ngstk split_list split_row_file batch_file
#' @importFrom data.table is.data.table
NULL

.onAttach <- function(libname, pkgname) {
  op <- options()
  # msg <- sprintf('annovarR %s \n', packageVersion('annovarR')) msg <-
  # sprintf('%s Documentation: ?annovarR, example(annovarR) and
  # browseVignettes(\'annovarR\')', msg) packageStartupMessage(msg)
  Sys.setenv(R_TESTS = "")
  invisible()
}

