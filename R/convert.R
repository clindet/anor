#' According the ANNOVAR convert2annovar.pl script, rewrite using R
#'
#' @param input.format format (default: pileup)
#' @param out.file Output file name (default: STDOUT)
#' @param snp.qual Quality score threshold in pileup file (default: 20)
#' @param snp.pvalue P-value threshold in GFF3-SOLiD file (default: 1)
#' @param coverage Read coverage threshold in pileup file (default: 0)
#' @param max.coverage Maximum coverage threshold (default: none)
#' @param include.info Include supporting information in output
#' @param chr Specify the chromosome (for CASAVA format)
#' @param chrmt chr identifier for mitochondria (default: M)
#' @param alt.cov Alternative allele coverage threshold (for pileup format)
#' @param allelic.frac Print out allelic fraction rather than het/hom status (for pileup format)
#' @param fraction Minimum allelic fraction to claim a mutation (for pileup format)
#' @param species If human, Convert chr23/24/25 to X/Y/M (for gff3-solid format)
#' @param filter.output Variants with this filter (case insensitive, for vcf4 format)
#' @param all.sample Process all samples in file with separate output files (for vcf4 format)
#' @param with.zyg print Zygosity/coverage/quality when -includeinfo is used (for vcf4 format)
#' @param genotype.qual Genotype quality score threshold (for vcf4 format)
#' @param variant.qual Variant quality score threshold (for vcf4 format)
#' @param comment Keep comment line in output (for vcf4 format)
#' @param dbsnp.file dbSNP file in UCSC format (for rsid format)
#' @param with.freq For --allsample, print frequency information instead (for vcf4 format)
#' @param seq.dir Directory with FASTA sequences (for region format)
#' @param ins.size Insertion size (for region format)
#' @param del.size Deletion size (for region format)
#' @param sub.size Substitution size (default: 1, for region format)
#' @param context Print context nucleotide for indels (for casava format)
#' @param verbose Ligical indicating wheather show the log message
#' @export
#' @examples
#' example.vcf <- system.file('extdata', 'demo/example.vcf', package = 'annovarR')
convert2annovar <- function(input.format = "pileup", out.file = NULL, snp.qual = 20, 
  snp.pvalue = 1, coverage = 0, max.coverage = NULL, include.info = NULL, chr = "", 
  chrmt = "M", alt.cov = NULL, allelic.frac = NULL, fraction = NULL, species = "human", 
  filter.output = NULL, all.sample = NULL, with.zyg = NULL, genotype.qual = NULL, 
  variant.qual = NULL, comment = NULL, dbsnp.file = NULL, with.freq = NULL, seq.dir = NULL, 
  ins.size = NULL, del.size = NULL, sub.size = NULL, context = NULL, verbose = FALSE) {
  iupac <- list(R = "AG", Y = "CT", S = "CG", W = "AT", K = "GT", M = "AC", A = "AA", 
    C = "CC", G = "GG", T = "TT", B = "CGT", D = "AGT", H = "ACT", V = "ACG", 
    N = "ACGT", . = "-", `-` = "-")
  iupac.rev <- rev(iupac)
  input.format <- check.par.input.format(input.format)
  check.par.all.sample(input.format = input.format, all.sample = all.sample, with.freq = with.freq)
  
}

# Check the validity of input.forat
check.par.input.format <- function(input.format = "pileup") {
  format.avail <- c("pileup", "cg", "cgmastervar", "gff3-solid", "soap", "maq", 
    "casava", "vcf4", "vcf4", "vcf4old", "rsid", "region", "transcript")
  if (input.format == "vcf") {
    input.format <- "vcf4"
  }
  if (!input.format %in% format.avail) {
    format.avail.str <- paste0(format.avail, collapse = ", ")
    stop(sprintf("Input format must in %s", format.avail.str))
  }
  return(input.format)
}

# Check the validity of all.sample
check.par.all.sample <- function(input.format = "vcf4", out.file = NULL, all.sample = NULL, 
  with.freq = NULL) {
  if (!is.null(all.sample)) {
    if (is.null(with.freq) && is.null(out.file)) {
      stop("Error in argument: please specify out.file when all.sample is specified (unless with.freq is set)")
    }
    if (input.format != "vcf4") {
      stop("Error in argument: the all.sample argument is supported only if input.format is 'vcf4'")
    }
    if (is.null(with.freq)) {
      info.msg(sprintf("NOTICE: output files will be written to %s", out.file))
    }
  }
}

# Some of function to convert anno.name

## ALL.2015.08 => name = hg19_ALL.sites.2015.08.txt, mongh = aug, year = 2015,
## region = all
convert.1000g.name <- function(anno.name) {
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
