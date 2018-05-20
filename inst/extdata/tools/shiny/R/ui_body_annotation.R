source("config.R")
annovar_annotation_names <- annovarR::get.annotation.names()
annovar_annotation_names <- annovar_annotation_names[
  stringr::str_detect(annovar_annotation_names, "perl_annovar")]
annovar_annotation_names <- annovar_annotation_names[annovar_annotation_names != "perl_annovar_merge"]
body_annotation_tabItem <- tabItem("annotation",
       tabsetPanel(type = 'pills',
         tabPanel('ANNOVAR',
           fluidRow(
              box(
                title = "ANNOVAR",
                width = 12,
                status = "primary",
                selectInput("annovar_input_file", label = "Input File", choices = c("",
                            featch_files()$file_path)),
                textAreaInput("annovar_input_file_area", label = "or Paste Variant Calls"),
                selectInput("annovar_input_file_genome", label = "Reference Genome", choices = c("hg18","hg19",
                            "hg38", "mm9", "mm10"), selected = "hg19"),
                selectInput("annovar_input_file_type", label = "Input Format",
                            choices = c("vcf", "vcf_old",
                                        "tsv", "avinput", "solid_gff3", "complete_genomics_masterVar")),
                selectInput("annovar_input_file_genome", label = "Gene Definition", choices = c("RefSeq Gene","UCSC Known Gene",
                            "ENSEMBL Gene", "GENECODE Gene"), selected = "RefSeq Gene"),
                selectInput("annovar_input_annotation_items", label = "Annotaion Databases",
                            choices = c("all", annovar_annotation_names), multiple = TRUE),
                actionButton("annovar_run", "Run")
              )
            )
          ),
         tabPanel('VEP',
           fluidRow(
             box(
               title = "VEP",
               width = 12,
               status = "primary",
               selectInput("vep_input_file", label = "Input File", choices = c("",
                             featch_files()$file_path)),
               textAreaInput("vep_input_file_area", label = "or Paste Variant Calls"),
               selectInput("vep_input_file_genome", label = "Reference Genome", choices = c("hg18","hg19",
                              "hg38", "mm9", "mm10"), selected = "hg19"),
               textAreaInput("vep_params", label = "VEP parameters",
                             value = suppressMessages(annovarR::vep(debug = TRUE))),
               actionButton("vep_run", "Run")
             )
           )
         ),
         tabPanel('vcfanno',
           fluidRow(
             box(
               title = "vcfanno",
               width = 12,
               status = "primary",
               selectInput("vcfanno_input_file", label = "Input File", choices = c("",
                  featch_files()$file_path)),
               textAreaInput("vcfanno_input_file_area", label = "or Paste Variant Calls"),
               selectInput("vcfanno_input_file_genome", label = "Reference Genome", choices = c("hg18","hg19",
                  "hg38", "mm9", "mm10"), selected = "hg19"),
               textAreaInput("vcfanno_params", label = "VEP parameters",
                             value = suppressMessages(annovarR::vcfanno(debug = TRUE))),
               actionButton("vcfanno_run", "Run")
             )
           )
          ),
         tabPanel('annovarR',
           fluidRow(
             box(
               title = "annovarR",
               width = 12,
               status = "primary",
               selectInput("annovarR_input_file", label = "Input File", choices = c("",
                  featch_files()$file_path)),
               textAreaInput("annovarR_input_file_area", label = "or Paste Variant Calls"),
               selectInput("annovarR_input_file_genome", label = "Reference Genome", choices = c("hg18","hg19",
                  "hg38", "mm9", "mm10"), selected = "hg19"),
               actionButton("annovarR_run", "Run")
             )
           )
         )
    )
)
