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
  if (!show.all.versions && !show.all.buildvers && !show.all.names) {
    if ((length(database.dir) == 1) && (length(name) > length(database.dir))) {
      if (!dir.exists(database.dir)) {
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
    all.versions <- install.bioinfo(name = name, github.cfg = github.cfg.null, 
      nongithub.cfg = database.cfg, show.all.versions = TRUE, verbose = verbose)
    return(all.versions)
  }
  filenames <- mapply(get.finished.filename, name = name, version = version, buildver = rep(buildver, 
    length(version)), database.cfg = rep(database.cfg, length(version)))
  filenames <- sprintf("%s/%s", database.dir, filenames)
  index <- file.exists(filenames) & file.size(filenames) > 0
  if (any(index)) {
    filenames <- paste0(filenames, collapse = ", ")
    info.msg(sprintf("%s already existed.", filenames), verbose = verbose)
    name <- name[!index]
    version <- version[!index]
  }
  if (length(name) == 0) {
    return(TRUE)
  }
  temp.download.dir = sprintf("%s/%s", tempdir(), stringi::stri_rand_strings(1, 
    10))
  info.msg(sprintf("Setted name:%s", name), verbose = verbose)
  info.msg(sprintf("Setted version:%s", version), verbose = verbose)
  info.msg(sprintf("Setted buildver:%s", buildver), verbose = verbose)
  info.msg(sprintf("Setted database.dir:%s", database.dir), verbose = verbose)
  info.msg(sprintf("Using %s as the temp install dir pass to BioInstaller::install.bioinfo.", 
    temp.download.dir), verbose = verbose)
  install.bioinfo(name = name, version = version, download.dir = temp.download.dir, 
    github.cfg = github.cfg.null, nongithub.cfg = database.cfg, download.only = FALSE, 
    extra.list = list(buildver = buildver), save.to.db = FALSE, verbose = verbose, 
    ...)
  files.and.dirs <- list.files(temp.download.dir, ".*")
  if (length(files.and.dirs) == 0) {
    info.msg(sprintf("Download %s %s version %s database fail.", buildver, version, 
      name), verbose = verbose)
    return(FALSE)
  } else {
    status <- file.copy(sprintf("%s/%s", temp.download.dir, files.and.dirs), 
      database.dir, overwrite = TRUE, recursive = TRUE)
    if (all(status)) {
      info.msg(sprintf("Download %s %s version %s database successful.", buildver, 
        version, name), verbose = verbose)
      return(TRUE)
    } else {
      info.msg(sprintf("Download %s %s version %s database fail.", buildver, 
        version, name), verbose = verbose)
      return(FALSE)
    }
  }
}

# Get download and decomparessd filename
get.finished.filename <- function(name, version, buildver = "hg38", database.cfg = "") {
  source_url <- eval.config("source_url", name, database.cfg, extra.list = list(buildver = buildver, 
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
