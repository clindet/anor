#' Download annovarR databases
#'
#' @param name Name of download eg. cosmic, avsnp, 1000g
#' @param version Version of download database
#' @param database.dir Dir of the databases
#' @param cfg Configuration file of annovarR databases infomation
#' @param verbose Logical indicating wheather print the extra log infomation
#' @param ... Other parameters pass to \code{\link[BioInstaller]{install.bioinfo}}
#' @export
#' @examples
#' download.database('1000g', database.dir = sprintf('%s/databases/', tempdir()), show.all.versions = TRUE)
download.database <- function(name = c(), version = c(), database.dir = c(), cfg = system.file("extdata", 
  "config/download.toml", package = "annovarR"), verbose = FALSE, ...) {
  if ((length(database.dir) == 1) && (length(name) > length(database.dir))) {
    database.dir <- rep(database.dir, length(name))
  }
  install.bioinfo(name = name, version = version, destdir = database.dir, github.cfg = "/dev/null", 
    nongithub.cfg = cfg, download.only = TRUE, verbose = verbose, ...)
}
