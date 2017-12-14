#' R utils function to run ANNOVAR tool. 
#'
#' @param perl Executable file of perl
#' @param cmd.pool Un-parsed commands of ANNOVAR
#' @param cmd.used Name in cmd.pool that used to parse final run command
#' @param down.dbname Need to download database name, e.g. avsnp147,cosmic70,1000g2015aug
#' @param input.file Input file name, e.g. example.avinput, example.vcf
#' @param annovar.dir ANNOVAR source code directory
#' @param buildver Genome version e.g. hg19, mm10
#' @param database.dir Database directory, e.g. /opt/annovar/humandb
#' @param webfrom Database resource warehouse, e.g. ucsc, annovar
#' @param anno.names ANNOVAR annotation names
#' @param out ANNOVAR -out parameter value 
#' @param convert.out ANNOVAR convert2annovar.pl output file, e.g. out.avinput
#' @param format ANNOVAR convert2annovar.pl input format option
#' @param operation.type Operation types used in table_annovar.pl
#' @param nastring ANNOVAR -nastring value, default is NA.
#' @param otherinfo Used in table_annovar.pl, -otherinfo
#' @param vcfinput Specify that input is in VCF format and output will be in VCF format, table_annovar.pl, -vcfinput
#' @param cmd.profix.flag Profix used in ANNOVAR command
#' @param extra.params Extra paramters in ANNOVAR command
#' @param debug If set TRUE, only print the command
#' @export
#' 
#' @examples
#' # original ANNOVAR download.database
#' down.dbname <- 'refGene'
#' annovar('perl', cmd.used = 'script1.downdb', down.dbname = 'avsnp147', 
#'         annovar.dir = '/opt/annovar', debug = TRUE)
#'
#' # ANNOVAR gene-based annotation
#' annovar('perl', cmd.used = 'script1.gene.based', input.file = 'example.avinput', 
#'         annovar.dir = '/opt/annovar', debug = TRUE)
#'
#' # ANNOVAR gene-based annotation
#' annovar('perl', cmd.used = 'script1.region.based', anno.names = 'cytoBand', 
#'         input.file = 'example.avinput', annovar.dir = '/opt/annovar', debug = TRUE)
#' 
#' # ANNOVAR filter-based annotation 
#' annovar('perl', cmd.used = 'script1.filter.based', anno.names = 'avsnp147', 
#'         input.file = 'example.avinput', annovar.dir = '/opt/annovar', debug = TRUE)
#' 
#' # ANNOVAR table_annovar.pl
#' anno.names <- c('refGene','cytoBand','genomicSuperDups','esp6500siv2_all', 
#' '1000g2015aug_all','1000g2015aug_afr','1000g2015aug_eas','1000g2015aug_eur', 
#' 'snp138','avsnp142','avsnp144','avsnp147','ljb26_all','cosmic70','cosmic81')
#' annovar('perl', cmd.used = 'script2', 
#'         anno.names = anno.names, 
#'         input.file = 'example.avinput', annovar.dir = '/opt/annovar', debug = TRUE)
#'
#' # ANNOVAR convert2annovar.pl
#' annovar('perl', cmd.used = 'script3', input.file = 'example.vcf', format = 'vcf4old', 
#'         convert.out = 'example.avinput', annovar.dir = '/opt/annovar', debug = TRUE)
annovar <- function(perl = Sys.which("perl"), cmd.pool = list(script1.downdb = paste(c("{perl}", 
  "{script}{extra.params}", "-downdb", "{buildver}", "{webfrom}", "{down.dbname}", 
  "{database.dir}"), collapse = " "), script1.gene.based = paste(c("{perl}", "{script}{extra.params}", 
  "{buildver}", "{input.file}", "{database.dir}"), collapse = " "), script1.region.based = paste(c("{perl}", 
  "{script}", " -regionanno{extra.params}", "{buildver}", "{anno.names}", "{input.file}", 
  "{database.dir}"), collapse = " "), script1.filter.based = paste(c("{perl}", 
  "{script}", "-filter{extra.params}", "{buildver}", "{anno.names}", "{input.file}", 
  "{database.dir}"), collapse = " "), script2 = paste(c("{perl}", "{script}", "{input.file}", 
  "{database.dir}", "{buildver}", "{out}", "-remove{extra.params}", "-protocol {anno.names}", 
  "-operation", "{operation}", "{nastring}", "{otherinfo}", "{vcfinput}"), collapse = " "), 
  script3 = paste("{perl}", "{script}{extra.params}", "-format", "{format}", "{input.file}", 
    "> {convert.out}", collapse = " ")), cmd.used = "script1.downdb", down.dbname = "", 
  input.file = "", annovar.dir = "", buildver = "hg19", database.dir = "{annovar.dir}/humandb", 
  webfrom = "annovar", anno.names = "", out = "", convert.out = "", format = "vcf4", 
  operation.type = list(gene.based = c("refGene", "knownGene", "ensGene", "ccdsGene"), 
    region.based = c("cytoBand", "genomicSuperDups")), cmd.profix.flag = list(buildver = "-buildver", 
    anno.names = "-dbtype", webfrom = "-webfrom", out = "-out", nastring = "-nastring"), 
  otherinfo = FALSE, nastring = "NA", vcfinput = FALSE, extra.params = "", debug = FALSE) {
  operation <- ""
  annotate.variation.pl <- sprintf("%s/annotate_variation.pl", annovar.dir)
  table.annovar.pl <- sprintf("%s/table_annovar.pl", annovar.dir)
  convert2annovar.pl <- sprintf("%s/convert2annovar.pl", annovar.dir)
  for (i in c("perl", "annovar.dir", "input.file", "convert.out", "annotate.variation.pl", 
    "table.annovar.pl", "convert2annovar.pl")) {
    assign(i, normalizePath(get(i), mustWork = FALSE))
  }
  if (cmd.used == "script2") {
    operation <- c()
    for (i in anno.names) {
      if (i %in% operation.type$gene.based) {
        operation <- c(operation, "g")
      } else if (i %in% operation.type$region.based) {
        operation <- c(operation, "r")
      } else {
        operation <- c(operation, "f")
      }
    }
    operation <- paste0(operation, collapse = ",")
  }
  for (i in names(cmd.profix.flag)) {
    if (cmd.used == "script2" && i == "anno.names") {
      next
    }
    if (!is.null(get(i)) && get(i) != "") {
      assign(i, paste(cmd.profix.flag[[i]], get(i), sep = " "))
    }
  }
  perl <- unname(perl)
  perl <- perl[perl != ""]
  if (length(perl) == 0) {
    perl <- "perl"
  }
  if (cmd.used %in% c("script1.downdb", "script1.gene.based", "script1.filter.based", 
    "script1.region.based")) {
    script <- annotate.variation.pl
  } else if (cmd.used == "script2") {
    script <- table.annovar.pl
  } else if (cmd.used == "script3") {
    script <- convert2annovar.pl
  }
  if (!file.exists(script) && !debug) {
    stop("Please set correct annovar.dir path!")
  }
  if (database.dir == "") {
    database.dir <- "{annovar.dir}/humandb"
  }
  database.dir <- glue(database.dir)
  database.dir <- normalizePath(database.dir, mustWork = FALSE)
  if (!file.exists(database.dir) && !debug) {
    stop("Please set correct database.dir path!")
  }
  anno.names <- paste0(anno.names, collapse = ",")
  for (i in c("vcfinput", "otherinfo")) {
    if (get(i)) {
      if (i == "vcfinput") {
        extra.params <- str_replace_all(extra.params, "--csvout", "")
      }
      assign(i, paste0("-", i))
    } else {
      assign(i, "")
    }
  }
  extra.list = list(perl = perl, script = script, input.file = input.file, annovar.dir = annovar.dir, 
    down.dbname = down.dbname, buildver = buildver, database.dir = database.dir, 
    webfrom = webfrom, anno.names = anno.names, out = out, format = format, extra.params = extra.params, 
    down.dbname = down.dbname, operation = operation, convert.out = convert.out, 
    otherinfo = otherinfo, nastring = nastring, vcfinput = vcfinput)
  cmd <- glue(cmd.pool[[cmd.used]])
  cat(cmd, sep = "\n")
  if (!debug) {
    system(cmd)
    return(cmd)
  } else {
    return(cmd)
  }
}
