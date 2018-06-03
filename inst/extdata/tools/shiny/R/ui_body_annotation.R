get_annotation_tabItem_ui <- function() {
  for(tool in annovarR_shiny_tools_list$annotation) {
    assign(sprintf("%s_boxes", tool),
           generate_boxes_object(get(sprintf("config.%s", tool)), tool))
  }

  body_annotation_tabItem <- tabItem("annotation",
                                     tabsetPanel(type = 'pills',
                                                 tabPanel('ANNOVAR',
                                                          fluidRow(eval(parse(text = paste0(unname(annovar_boxes), collapse = ","))))),
                                                 #tabPanel('VEP',
                                                 #   fluidRow(eval(parse(text = paste0(unname(vep_boxes), collapse = ","))))),
                                                 tabPanel('vcfanno',
                                                          fluidRow(eval(parse(text = paste0(unname(vcfanno_boxes), collapse = ","))))),
                                                 tabPanel('annovarR',
                                                          fluidRow(eval(parse(text = paste0(unname(annovarR_boxes), collapse = ",")))))
                                     )
  )
}
