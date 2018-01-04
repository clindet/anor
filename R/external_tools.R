#' R function to run ANNOVAR. 
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
    os <- Sys.info()["sysname"][[1]]
    if (os == "Windows") {
      Sys.setenv(PATH = paste0(Sys.getenv("PATH"), ";", annovar.dir))
    }
    status <- system(cmd)
    attr(cmd, "status") <- status
    return(cmd)
  } else {
    return(cmd)
  }
}

#' R function to run VEP
#'
#' @param vep Executable file of vep
#' @param cache Enables use of the cache. Add --refseq or 
#' --merged to use the refseq or merged cache, (if installed).
#' @param cache_version Use a different cache version than the assumed default (the VEP version). 
#' This should be used with Ensembl Genomes caches since their version numbers do not 
#' match Ensembl versions. For example, the VEP/Ensembl version may be 88 and the 
#' Ensembl Genomes version 35. Not used by default
#' @param offline Enable offline mode. No database connections will be made, 
#' and a cache file or GFF/GTF file is required for annotation. 
#' Add --refseq to use the refseq cache (if installed). Not used by default
#' @param buildver Select the assembly version to use if more than one available. 
#' If using the cache, you must have the appropriate assembly's cache file installed. 
#' If not specified and you have only 1 assembly version installed, this will be chosen by default. 
#' Default = use found assembly version (GRch37)
#' @param input.file Input file name. If not specified, the script will attempt to read from STDIN.
#' @param dir Specify the base cache/plugin directory to use. Default = '$HOME/.vep/'
#' @param out Output file name. The script can write to STDOUT by 
#' specifying STDOUT as the output file name - this will force quiet mode. 
#' Default = 'variant_effect_output.txt'
#' @param fasta Specify a FASTA file or a directory containing FASTA files to use to look up reference sequence. 
#' The first time you run the script with this parameter an index will be built 
#' which can take a few minutes. This is required if fetching HGVS annotations (--hgvs) 
#' or checking reference sequences (--check_ref) in offline mode (--offline), 
#' and optional with some performance increase in cache mode (--cache). 
#' See documentation for more details. Not used by default
#' @param everything    Shortcut flag to switch on all of the following:
#' --sift b, --polyphen b, --ccds, --uniprot, --hgvs, --symbol, --numbers, 
#' --domains, --regulatory, --canonical, --protein, --biotype, --uniprot, 
#' --tsl, --appris, --gene_phenotype --af, --af_1kg, --af_esp, --af_gnomad, 
#' --max_af, --pubmed, --variant_class
#' @param extra.params Extra paramters in vep command
#' @param debug If set TRUE, only print the command
#' @export 
#' @examples
#' vep(debug = TRUE)
vep <- function(vep = Sys.which("vep"), cache = TRUE, cache_version = 91, offline = TRUE, 
  buildver = "GRCh37", dir = file.path(Sys.getenv("HOME"), ".vep"), input.file = "", 
  out = "variant_effect_output.txt", fasta = "", everything = TRUE, extra.params = "", 
  debug = FALSE) {
  if (!file.exists(vep) & !debug) {
    stop("Please set correctly VEP path.")
  } else if (debug) {
    vep <- "vep"
  }
  alias_var = list(buildver = "assembly", input.file = "input_file", out = "output_file")
  for (i in names(alias_var)) {
    assign(alias_var[[i]], get(i))
  }
  var <- c("cache_version", "assembly", "dir", "output_file", "input_file")
  flag_var <- c("cache", "offline", "everything")
  cmd <- vep
  for (i in var) {
    if (!is.null(get(i)) && get(i) != "") {
      cmd <- paste(cmd, paste0("--", i), get(i), sep = " ")
    }
  }
  for (i in flag_var) {
    if (get(i)) {
      cmd <- paste0(cmd, " --", i)
    }
  }
  cmd <- sprintf("%s %s", cmd, extra.params)
  cat(cmd, sep = "\n")
  if (debug) {
    return(cmd)
  } else {
    status <- system(cmd)
    attr(cmd, "status") <- status
    return(cmd)
  }
}

#' R function to run vcfanno
#' 
#' @param vcfanno Executable file of vcfanno (Download from https://github.com/brentp/vcfanno/releases)
#' @param vcfanno.database.cfg vcfanno required database configuration file 
#' (Not the annovarR database.cfg)
#' @param base_path Optional base_path to prepend to annotation files in the config
#' @param lua Optional path to a file containing custom javascript functions to be used as ops
#' @param ends Annotate the start and end as well as the interval itself.
#' @param input.file Input file path (VCF only)
#' @param out Output file path
#' @param thread number of processes to use. (default 2)
#' @param permissive_overlap annotate with an overlapping variant even it doesn't share the same ref and alt alleles. 
#' Default is to require exact match between variants.
#' @param debug If set TRUE, only print the command
#' @export
#' @examples
#' vcfanno(debug = TRUE)
vcfanno <- function(vcfanno = Sys.which(c("vcfanno", "vcfanno_osx", "vcfanno_linux64")), 
  vcfanno.database.cfg = system.file("extdata", "demo/vcfanno_demo/conf.toml", 
    package = "annovarR"), base_path = "", lua = "", ends = FALSE, input.file = "input.vcf", 
  out = "output.vcf", thread = 2, permissive_overlap = FALSE, debug = FALSE) {
  vcfanno <- vcfanno[vcfanno != ""][1]
  if (!file.exists(vcfanno) & !debug) {
    stop("Please set correctly vcfanno path.")
  } else if (debug) {
    vcfanno <- "vcfanno_linux64"
  }
  if (out != "" && !dir.exists(dirname(out))) {
    dir.create(dirname(out))
  }
  alias_var = list(thread = "p")
  for (i in names(alias_var)) {
    assign(alias_var[[i]], get(i))
  }
  var <- c("base_path", "lua", "p")
  flag_var <- c("ends", "permissive_overlap")
  cmd <- vcfanno
  for (i in var) {
    if (!is.null(get(i)) && get(i) != "") {
      cmd <- paste(cmd, paste0("-", str_replace(i, "_", "-")), get(i), sep = " ")
    }
  }
  for (i in flag_var) {
    if (get(i)) {
      cmd <- paste0(cmd, " -", i)
    }
  }
  cmd <- sprintf("%s %s %s > %s", cmd, vcfanno.database.cfg, input.file, out)
  cat(cmd, sep = "\n")
  if (debug) {
    return(cmd)
  } else {
    status <- system(cmd)
    attr(cmd, "status") <- status
    return(cmd)
  }
}
