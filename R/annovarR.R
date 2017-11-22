#' annovarR is an efficient software tool to utilize update-to-date information to 
#' functionally annotate genetic variants detected from diverse genomes based on ANNOVAR.
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
#' @importFrom ngstk split_list
NULL

.onAttach <- function(libname, pkgname) {
  op <- options()
  # msg <- sprintf('annovarR %s \n', packageVersion('annovarR')) msg <-
  # sprintf('%s Documentation: ?annovarR, example(annovarR) and
  # browseVignettes(\'annovarR\')', msg) packageStartupMessage(msg)
  Sys.setenv(R_TESTS = "")
  invisible()
}

