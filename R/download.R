#' Download annovarR databases
#'
#' @param name Name of download eg. cosmic, avsnp, 1000g
#' @param version Version of download database
#' @param buildver Genome version, e.g hg19, hg38, mm10
#' @param database.dir Dir of the databases
#' @param database.cfg Configuration file of annovarR databases infomation
#' @param show.all.versions Logical indicating wheather show all avaliable versions
#' @param show.all.names Logical indicating wheather show all avaliable name
#' @param show.all.buildvers Logical indicating wheather show all avaliable buildver
#' @param verbose Logical indicating wheather print the extra log infomation
#' @param ... Other parameters pass to \code{\link[BioInstaller]{install.bioinfo}}
#' @export
#' @examples
#' download.database('1000g', database.dir = sprintf('%s/databases/', tempdir()), 
#' show.all.versions = TRUE)
download.database <- function(name = c(), version = c(), buildver = "hg19", database.dir = c(), 
  database.cfg = system.file("extdata", "config/download.toml", package = "annovarR"), 
  show.all.versions = FALSE, show.all.names = FALSE, show.all.buildvers = FALSE, 
  verbose = FALSE, ...) {
  if (!show.all.versions && !show.all.buildvers && !show.all.names){
    if ((length(database.dir) == 1) && (length(name) > length(database.dir))) {
      if (!dir.exists(database.dir)){
        dir.create(database.dir, recursive = TRUE)
      }
      database.dir <- rep(database.dir, length(name))
    } else {
      dirs <- database.dir[!dir.exists(database.dir)]
      if (length(dirs) > 0) {
        dir.create(dirs, recursive = TRUE)
      }
    }
  }
  if (show.all.buildvers) {
    buildvers <- eval.config(value = "buildver_available", config = name, file = database.cfg)
    return(buildvers)
  }
  github.cfg.null <- tempfile()
  file.create(github.cfg.null)
  if (show.all.names) {
    all.names <- install.bioinfo(github.cfg = github.cfg.null, nongithub.cfg = database.cfg, 
      show.all.names = TRUE, verbose = verbose)
    return(all.names)
  }
  if (show.all.versions) {
    all.versions <- install.bioinfo(name = name, github.cfg = github.cfg.null, nongithub.cfg = database.cfg, 
      show.all.versions = TRUE, verbose = verbose)
    return(all.versions)
  }
  temp.download.dir = sprintf("%s/%s", tempdir(),stringi::stri_rand_strings(1, 10))
  install.bioinfo(name = name, version = version, destdir = temp.download.dir, 
    github.cfg = github.cfg.null, nongithub.cfg = database.cfg, download.only = FALSE, 
    extra.list = list(buildver = buildver), save.to.db = FALSE, verbose = verbose, ...)
  files.and.dirs <- list.files(temp.download.dir, ".*")
  if (length(files.and.dirs) == 0) {
    info.msg(sprintf("Download %s %s version %s database fail.", buildver, version, name), verbose = verbose)
    return(FALSE)
  } else{
    status <- file.copy(sprintf("%s/%s", temp.download.dir, files.and.dirs), database.dir, overwrite = TRUE, recursive = TRUE)
    if (all(status)) {
      info.msg(sprintf("Download %s %s version %s database successful.", buildver, version, name), verbose = verbose)
      return(TRUE)
    } else {
      info.msg(sprintf("Download %s %s version %s database fail.", buildver, version, name), verbose = verbose)
      return(FALSE)
    }
  }
}
