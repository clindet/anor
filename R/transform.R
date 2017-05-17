reform.1000g <- function(dat = "", filename = "", ...) {
  if (filename != "") {
    dat <- fread(filename, ...)
  }
  dat <- as.data.frame(dat)
  end <- as.numeric(dat$V2)
  have.num <- str_detect(dat$V4, "[0-9]")
  end[have.num] <- end[have.num] + as.numeric(str_extract(dat$V4[have.num], "[0-9]*"))
  dat <- cbind(dat[, c(1, 2)], end, dat[, c(3, 4, 6, 5)])
  only.num <- !str_detect(dat$V4, "[atcgATCG]")
  dat$end[only.num] <- dat$end[only.num] - 1
  dat$V4[only.num] <- "-"
  zero.alt <- str_detect(dat$V4, "0[atctATCG]*")
  dat$V4[zero.alt] <- str_replace(dat$V4[zero.alt], "0", "")
  dat$V4[zero.alt] <- str_replace(dat$V4[zero.alt], dat$V3[zero.alt], "")
  dat$V3[zero.alt] <- "-"
  have.num.but.no.only <- str_detect(dat$V4, "[1-9]")
  have.num.but.no.only <- have.num.but.no.only & !only.num
  dat$end[have.num.but.no.only] <- dat$end[have.num.but.no.only] - 1
  num <- str_extract(dat$V4, "[1-9]*")
  dat$V4[have.num.but.no.only] <- str_replace_all(dat$V4[have.num.but.no.only], 
    "[0-9]*", "")
  ref.len <- str_length(dat$V3)
  alt.len <- str_length(dat$V4)
  is.deletion <- ref.len > alt.len
  colnames(dat) <- c("chr", "start", "end", "ref", "alt", "rs", "frq")
  return(dat)
}
