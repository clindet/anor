source("config.R")
# Generate maftools UI

maftools_boxes <- generate_boxes_object(config.maftools, "maftools")

gvmap_boxes <- generate_boxes_object(config.gvmap, "gvmap")

clusterProfiler_boxes <- generate_boxes_object(config.clusterProfiler, "clusterProfiler")

cmd <- sprintf(paste0("body_visulization_tabItem <- tabItem('visulization',",
                   "tabsetPanel(type = 'pills',",
                      "tabPanel('maftools',",
                        "fluidRow( %s )",
                      "),",
                      "tabPanel('gvmap',",
                        "fluidRow( %s )",
                      "),",
                      "tabPanel('clusterProfiler',",
                        "fluidRow( %s )",
                      ")",
                   ")",
                  ")"), paste0(unname(maftools_boxes), collapse = ","),
               paste0(unname(gvmap_boxes), collapse = ","),
               paste0(unname(clusterProfiler_boxes), collapse = ","))

body_visulization_tabItem <- eval(parse(text = cmd))
