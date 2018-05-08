body_visulization_tabItem <- tabItem("dashboard",
      tabsetPanel(type = "pills",
        tabPanel("maftools",
          fluidRow(
            box(title = "Output of maftools fields summary",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsed = TRUE,
                collapsible = TRUE,
                shiny::verbatimTextOutput(outputId = "maftools_fields_summary")
            ),
            box(title = "Output of maftools sample summary",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsed = TRUE,
                collapsible = TRUE,
                shiny::dataTableOutput(outputId = "maftools_sample_summary")

            ),
            box(title = "Output of maftools gene summary",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsed = TRUE,
                collapsible = TRUE,
                shiny::dataTableOutput(outputId = "maftools_gene_summary")
            ),
            box(title = "Output of maftools clinical data summary",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                shiny::dataTableOutput(outputId = "maftools_clinical_data")
            ),
            box(title = "Output of maftools MAF summary",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                shiny::plotOutput(outputId = "maftools_plot_maf_summary"),
                shiny::actionButton("draw_maftools_plot_maf_summary", label = "Plot"),
                shiny::downloadButton("export_maftools_plot_maf_summary", label = "Export PDF")
            ),
            box(title = "Output of maftools oncoplots",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                shiny::plotOutput(outputId = "maftools_plot_oncoplots"),
                shiny::actionButton("draw_maftools_plot_oncoplots", label = "Plot"),
                shiny::downloadButton("export_maftools_plot_oncoplots", label = "Export PDF")
            ),
            box(title = "Output of maftools oncoplots with copy number data",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                shiny::plotOutput(outputId = "maftools_plot_oncoplots_cnv"),
                shiny::actionButton("draw_maftools_plot_oncoplots_cnv", label = "Plot"),
                shiny::downloadButton("export_maftools_plot_oncoplots_cnv", label = "Export PDF")
            ),
            box(title = "Output of maftools oncoplots with advanced",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                shiny::plotOutput(outputId = "maftools_plot_oncoplots_advanced"),
                shiny::actionButton("draw_maftools_plot_oncoplots_advanced", label = "Plot"),
                shiny::downloadButton("export_maftools_plot_oncoplots_advanced", label = "Export PDF")
            ),
            box(title = "Output of maftools transition and transversions",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                shiny::plotOutput(outputId = "maftools_plot_titv"),
                shiny::actionButton("draw_maftools_plot_titv", label = "Plot"),
                shiny::downloadButton("export_maftools_plot_titv", label = "Export PDF")
            ),
            box(title = "Output of maftools Lollipop plots for amino acid changes",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                shiny::plotOutput(outputId = "maftools_plot_lollipop"),
                shiny::actionButton("draw_maftools_plot_lollipop", label = "Plot"),
                shiny::downloadButton("export_maftools_plot_lollipop", label = "Export PDF")
            )
          )
        ),
        tabPanel("Complexheatmap",
          fluidRow(
            box(title = "Output of maftools fields summary",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              collapsed = TRUE,
              collapsible = TRUE
            )
          )
        )
      )
    )
