maftools_server <- function(input, output){
 # maftools-object-summary
  laml.maf = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools') #path to TCGA LAML MAF file
  laml.clin = system.file('extdata', 'tcga_laml_annot.tsv', package = 'maftools') # clinical information containing survival information and histology. This is optional
  all.lesions <- system.file("extdata", "all_lesions.conf_99.txt", package = "maftools")
  amp.genes <- system.file("extdata", "amp_genes.conf_99.txt", package = "maftools")
  del.genes <- system.file("extdata", "del_genes.conf_99.txt", package = "maftools")
  scores.gis <- system.file("extdata", "scores.gistic", package = "maftools")
  laml.plus.gistic = read.maf(maf = laml.maf, gisticAllLesionsFile = all.lesions, gisticAmpGenesFile = amp.genes, gisticDelGenesFile = del.genes, gisticScoresFile = scores.gis, isTCGA = TRUE)
  laml = read.maf(maf = laml.maf, clinicalData = laml.clin)

  output$maftools_fields_summary <- shiny::renderPrint(getFields(laml))
  output$maftools_sample_summary <- shiny::renderDataTable(
    getSampleSummary(laml), options = list(
      pageLength = 10,
      lengthMenu = list(list(5, 10, 25, 50, -1), list(5, 10 , 25, 50, "All")),
      dom = 'Bfrtlip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  output$maftools_gene_summary <- shiny::renderDataTable(
    getGeneSummary(laml), options = list(
      pageLength = 10,
      lengthMenu = list(list(5, 10, 25, 50, -1), list(5, 10 , 25, 50, "All")),
      dom = 'Bfrtlip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  output$maftools_clinical_data <- shiny::renderDataTable(
    getClinicalData(laml), options = list(
      pageLength = 10,
      lengthMenu = list(list(5, 10, 25, 50, -1), list(5, 10 , 25, 50, "All")),
      dom = 'Bfrtlip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))

  observeEvent(input$draw_maftools_plot_maf_summary, {
    output$maftools_plot_maf_summary <- shiny::renderPlot(plotmafSummary(maf = laml,
       rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE)
    )
  })
  observeEvent(input$draw_maftools_plot_oncoplots, {
    output$maftools_plot_oncoplots <- shiny::renderPlot(
       oncoplot(maf = laml, top = 10, fontSize = 12)
    )
  })

  observeEvent(input$draw_maftools_plot_oncoplots_cnv, {
    output$maftools_plot_oncoplots_cnv <- shiny::renderPlot(
      oncoplot(maf = laml.plus.gistic, top = 10, fontSize = 12)
    )
  })
  observeEvent(input$draw_maftools_plot_oncoplots_advanced, {
    # for maftools_plot_oncoplots_advanced
    col = RColorBrewer::brewer.pal(n = 8, name = 'Paired')
    names(col) = c('Frame_Shift_Del','Missense_Mutation', 'Nonsense_Mutation', 'Multi_Hit', 'Frame_Shift_Ins',
                   'In_Frame_Ins', 'Splice_Site', 'In_Frame_Del')

    #Color coding for FAB classification; try getAnnotations(x = laml) to see available annotations.
    fabcolors = RColorBrewer::brewer.pal(n = 8,name = 'Spectral')
    names(fabcolors) = c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7")
    fabcolors = list(FAB_classification = fabcolors)

    #MutSig reusults
    laml.mutsig <- system.file("extdata", "LAML_sig_genes.txt.gz", package = "maftools")
    output$maftools_plot_oncoplots_advanced <- shiny::renderPlot(
      oncoplot(maf = laml, colors = col, mutsig = laml.mutsig, mutsigQval = 0.01,
      clinicalFeatures = 'FAB_classification', sortByAnnotation = TRUE,
      annotationColor = fabcolors)
    )
  })
  observeEvent(input$draw_maftools_plot_titv, {
    laml.titv = titv(maf = laml, plot = FALSE, useSyn = TRUE)
    #plot titv summary
    output$maftools_plot_titv <- shiny::renderPlot(plotTiTv(res = laml.titv))
  })

  observeEvent(input$draw_maftools_plot_lollipop, {
    #plot titv summary
    output$maftools_plot_lollipop <- shiny::renderPlot(
      lollipopPlot(maf = laml, gene = 'DNMT3A', AACol = 'Protein_Change',
                   showMutationRate = TRUE)
    )
  })

  output$export_maftools_plot_maf_summary <- downloadHandler(sprintf("%s.pdf", stringi::stri_rand_strings(1, 10)),
    function(theFile) {
    make_pdf <- function(filename) {
      Cairo(type = 'pdf', file = filename, width = 21, height = 14, units='cm', bg='transparent')
      plotmafSummary(maf = laml, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE)
      dev.off()
    }
    make_pdf(theFile)
  })

  output$export_maftools_plot_oncoplots <- downloadHandler(sprintf("%s.pdf", stringi::stri_rand_strings(1, 10)),
    function(theFile) {
      make_pdf <- function(filename) {
      Cairo(type = 'pdf', file = filename, width = 21, height = 14, units='cm', bg='transparent')
      oncoplot(maf = laml, top = 10, fontSize = 12)
      dev.off()
    }
    make_pdf(theFile)
  })

  output$export_maftools_plot_oncoplots_cnv <- downloadHandler(sprintf("%s.pdf", stringi::stri_rand_strings(1, 10)),
    function(theFile) {
      make_pdf <- function(filename) {
      Cairo(type = 'pdf', file = filename, width = 21, height = 14, units='cm', bg='transparent')
      oncoplot(maf = laml.plus.gistic, top = 10, fontSize = 12)
      dev.off()
    }
    make_pdf(theFile)
  })
 output$export_maftools_plot_oncoplots_advanced <- downloadHandler(sprintf("%s.pdf", stringi::stri_rand_strings(1, 10)),
    function(theFile) {
      make_pdf <- function(filename) {
        # for maftools_plot_oncoplots_advanced
        col = RColorBrewer::brewer.pal(n = 8, name = 'Paired')
        names(col) = c('Frame_Shift_Del','Missense_Mutation', 'Nonsense_Mutation', 'Multi_Hit', 'Frame_Shift_Ins',
                       'In_Frame_Ins', 'Splice_Site', 'In_Frame_Del')

        #Color coding for FAB classification; try getAnnotations(x = laml) to see available annotations.
        fabcolors = RColorBrewer::brewer.pal(n = 8,name = 'Spectral')
        names(fabcolors) = c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7")
        fabcolors = list(FAB_classification = fabcolors)

        #MutSig reusults
        laml.mutsig <- system.file("extdata", "LAML_sig_genes.txt.gz", package = "maftools")

        Cairo(type = 'pdf', file = filename, width = 21, height = 27, units='cm', bg='transparent')
        oncoplot(maf = laml, colors = col, mutsig = laml.mutsig, mutsigQval = 0.01,
                   clinicalFeatures = 'FAB_classification', sortByAnnotation = TRUE,
                   annotationColor = fabcolors)
        dev.off()
    }
    make_pdf(theFile)
 })

  output$export_maftools_plot_titv <- downloadHandler(sprintf("%s.pdf", stringi::stri_rand_strings(1, 10)),
    function(theFile) {
      make_pdf <- function(filename) {
      laml.titv = titv(maf = laml, plot = FALSE, useSyn = TRUE)
      Cairo(type = 'pdf', file = filename, width = 21, height = 17, units='cm', bg='transparent')
      plotTiTv(res = laml.titv)
      dev.off()
    }
    make_pdf(theFile)
 })

  output$export_maftools_plot_lollipop <- downloadHandler(sprintf("%s.pdf", stringi::stri_rand_strings(1, 11)),
    function(theFile) {
      make_pdf <- function(filename) {
      Cairo(type = 'pdf', file = filename, width = 21, height = 17, units='cm', bg='transparent')

      lollipopPlot(maf = laml, gene = 'DNMT3A', AACol = 'Protein_Change',
               showMutationRate = TRUE)
      dev.off()
      file.copy(filename, "~/out.pdf")
    }
    make_pdf(theFile)
 })
  return(output)
}

