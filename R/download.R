#' Download annovarR databases
#'
#' @param download.name Download name, default is NULL and get value from database.cfg 
#' dependence_db
#' @param version Version of download database, supported version can be get by 
#' download.database('db_annovar_avsnp', show.all.versions = TRUE)
#' @param buildver Genome version, e.g hg19, hg38, mm10
#' @param database.dir Dir of the databases
#' @param download.cfg Configuration file for download, most of annovarR database 
#' resources can be found in system.file('extdata', 
#' 'config/db/db_annovar.toml', package = 'BioInstaller')
#' @param show.all.versions Logical indicating wheather show all avaliable versions
#' @param show.all.names Logical indicating wheather show all avaliable name
#' @param show.all.buildvers Logical indicating wheather show all avaliable buildver
#' @param verbose Logical indicating wheather print the extra log infomation
#' @param ... Other parameters pass to \code{\link[BioInstaller]{install.bioinfo}}
#' @export
#' @examples
#' download.database('db_annovar_avsnp', database.dir = sprintf('%s/databases/', tempdir()), 
#' show.all.versions = TRUE)
download.database <- function(download.name = NULL, version = c(), buildver = "hg19", 
  database.dir = c(), download.cfg = system.file("extdata", "config/db/db_annovar.toml", 
    package = "BioInstaller"), show.all.versions = FALSE, show.all.names = FALSE, 
  show.all.buildvers = FALSE, verbose = FALSE, ...) {
  if (!is.null(database.dir)) {
    database.dir <- normalizePath(database.dir, "/", mustWork = FALSE)
  }
  if (!show.all.versions && !show.all.buildvers && !show.all.names) {
    if ((length(database.dir) == 1) && (length(download.name) > length(database.dir))) {
      if (!dir.exists(database.dir)) {
        dir.create(database.dir, recursive = TRUE)
      }
      database.dir <- rep(database.dir, length(download.name))
    } else {
      dirs <- database.dir[!dir.exists(database.dir)]
      if (length(dirs) > 0) {
        dir.create(dirs, recursive = TRUE)
      }
    }
  }
  if (show.all.buildvers) {
    buildvers <- eval.config(value = "buildver_available", config = download.name, 
      file = download.cfg)
    return(buildvers)
  }
  github.cfg.null <- tempfile()
  write.config(list(title = "empty github.cfg"), github.cfg.null)
  if (show.all.names) {
    all.names <- install.bioinfo(github.cfg = github.cfg.null, nongithub.cfg = download.cfg, 
      show.all.names = TRUE, verbose = verbose)
    return(all.names)
  }
  all.versions <- install.bioinfo(name = download.name, github.cfg = github.cfg.null, 
    nongithub.cfg = download.cfg, show.all.versions = TRUE, verbose = verbose)
  if (show.all.versions) {
    return(all.versions)
  }
  if (length(version) == 0) {
    version = all.versions[1]
  }
  filenames <- mapply(get.finished.filename, download.name = download.name, version = version, 
    buildver = rep(buildver, length(version)), download.cfg = rep(download.cfg, 
      length(version)))
  filenames <- sprintf("%s/%s", database.dir, filenames)
  index <- file.exists(filenames) & file.size(filenames) > 0
  if (any(index)) {
    filenames <- paste0(filenames, collapse = ", ")
    info.msg(sprintf("%s already existed.", filenames), verbose = verbose)
    download.name <- download.name[!index]
    version <- version[!index]
  }
  if (length(download.name) == 0) {
    return(TRUE)
  }
  temp.download.dir = sprintf("%s/%s", database.dir, stringi::stri_rand_strings(1, 
    10))
  info.msg(sprintf("Setted download.name:%s", download.name), verbose = verbose)
  info.msg(sprintf("Setted version:%s", version), verbose = verbose)
  info.msg(sprintf("Setted buildver:%s", buildver), verbose = verbose)
  info.msg(sprintf("Setted database.dir:%s", database.dir), verbose = verbose)
  info.msg(sprintf("Using %s as the temp install dir pass to BioInstaller::install.bioinfo.", 
    temp.download.dir), verbose = verbose)
  install.bioinfo(name = download.name, version = version, download.dir = temp.download.dir, 
    github.cfg = github.cfg.null, nongithub.cfg = download.cfg, download.only = FALSE, 
    extra.list = list(buildver = buildver), save.to.db = FALSE, verbose = verbose, 
    ...)
  files.and.dirs <- list.files(temp.download.dir, ".*")
  if (length(files.and.dirs) == 0) {
    info.msg(sprintf("Download %s %s version %s database fail.", buildver, version, 
      download.name), verbose = verbose)
    return(FALSE)
  } else {
    status <- file.rename(sprintf("%s/%s", temp.download.dir, files.and.dirs), 
      sprintf("%s/%s", database.dir, files.and.dirs))
    unlink(temp.download.dir, recursive = TRUE, force = TRUE)
    if (all(status)) {
      info.msg(sprintf("Download %s %s version %s database successful.", buildver, 
        version, download.name), verbose = verbose)
      return(TRUE)
    } else {
      info.msg(sprintf("Download %s %s version %s database fail.", buildver, 
        version, download.name), verbose = verbose)
      return(FALSE)
    }
  }
}

#' Use annotation name to get download.name that can be used 
#' to download the database use \code{{download.database}}
#' @param anno.name Annotation name, eg. avsnp138, avsnp147, 1000g2015aug_all
#' @param database.cfg Configuration file of annovarR databases infomation
#' @export
#' @examples
#' get.download.name('avsnp147')
get.download.name <- function(anno.name = "", database.cfg = system.file("extdata", 
  "config/databases.toml", package = "annovarR")) {
  download.name <- get.cfg.value.by.name(anno.name, database.cfg, key = "dependence_db")
}

# Get download and decomparessd filename
get.finished.filename <- function(download.name = "", version = "", buildver = "hg38", 
  download.cfg = "") {
  source_url <- eval.config("source_url", download.name, download.cfg, extra.list = list(buildver = buildver, 
    version = version))[1]
  filename <- basename(source_url)
  filename <- str_replace(filename, ".gz$", "")
  if (str_detect(version, "^1000g")) {
    prefix <- str_split(filename, fixed("."))[[1]]
    prefix <- prefix[length(prefix)]
    if (prefix == "zip") {
      prefix <- "txt"
    } else if (prefix == "tar") {
      prefix <- "sql"
    }
    filename <- set.1000g.db(sprintf("%s_all", version), buildver, "", prefix)
    filename <- basename(filename)
  }
  return(filename)
}
