## ALL.2015.08 => name = hg19_ALL.sites.2015.08.txt, mongh = aug, year = 2015,
## region = all
convert.1000g.name <- function(anno.name = "") {
  month.hash <- list(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", 
    jun = "06", jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")
  month <- str_extract(anno.name, names(month.hash))
  month <- month[!is.na(month)]
  month <- month.hash[month]
  year <- str_extract(anno.name, "1000g[0-9]*")
  year <- str_replace(year, "1000g", "")
  region <- str_extract(anno.name, "_[a-z]*")
  region <- toupper(str_replace(region, "_", ""))
  return(list(anno.name = anno.name, month = month, year = year, region = region))
}
