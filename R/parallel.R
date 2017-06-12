#' Annotation function (mulitple name)
#'
#' @param row.cl Core be used (default is 1) for split data by row
#' @param ... Other parametes see \code{\link{annotation.merge}}
#' @export
#' @examples
#' library(data.table)
#' chr <- c('chr1', 'chr2', 'chr1')
#' start <- c('10020', '10020', '10020')
#' end <- c('10020', '10020', '10020')
#' ref <- c('A', 'A', 'A')
#' alt <- c('-', '-', '-')
#' database <- system.file('extdata', 'demo/hg19_avsnp147.txt', package = 'annovarR')
#' database.dir <- dirname(database)
#' dat <- data.table(chr = chr, start = start, end = end, ref = ref, alt = alt)
#' library(parallel)
#' row.cl <- makeCluster(2)
#' x <- parAnnotation(dat = dat, anno.names = c('avsnp147', '1000g2015aug_all'), 
#' database.dir = database.dir, db.type = 'txt', row.cl = row.cl)
#' stopCluster(row.cl)
parAnnotation <- function(row.cl, ...) {
  row.cl.num <- length(row.cl)
  registerDoParallel(row.cl)
  clusterEvalQ(row.cl, library(annovarR))
  params <- list(...)
  dat <- params$dat
  dat <- splitList(dat, row.cl.num)
  clusterExport(row.cl, "row.cl.num", envir = environment())
  clusterExport(row.cl, "params", envir = environment())
  clusterExport(row.cl, "dat", envir = environment())
  x <- NULL
  result <- foreach(x = 1:row.cl.num, .packages = "annovarR") %dopar% {
    params.pre <- params
    params.pre[[1]] <- NULL
    params <- params.pre
    dat.pre <- list()
    dat.pre["dat"] <- dat[x]
    params["dat"] <- dat.pre["dat"]
    params$dat <- as.data.table(params$dat)
    result <- do.call(annotation.merge, params)
  }
  result <- do.call(rbindlist, list(result))
  return(result)
}

# Function to split list and distributed to different computing core len is the
# number of core X is the list object
splitList <- function(X, len) {
  len <- ceiling(length(X[[1]])/len)
  a <- seq(from = 1, to = length(X[[1]]), by = len)
  b <- seq(from = len, to = length(X[[1]]), by = len)
  if (length(b) < length(a)) {
    b <- c(b, length(X[[1]]))
  }
  result <- NULL
  for (i in 1:length(a)) {
    result[[i]] <- lapply(X, function(x) x[a[i]:b[i]])
  }
  return(result)
}
