return(output)

# maftools-object-summary
laml.maf = system.file("extdata", "tcga_laml.maf.gz", package = "maftools")  #path to TCGA LAML MAF file
laml.clin = system.file("extdata", "tcga_laml_annot.tsv", package = "maftools")  # clinical information containing survival information and histology. This is optional
all.lesions <- system.file("extdata", "all_lesions.conf_99.txt", package = "maftools")
amp.genes <- system.file("extdata", "amp_genes.conf_99.txt", package = "maftools")
del.genes <- system.file("extdata", "del_genes.conf_99.txt", package = "maftools")
scores.gis <- system.file("extdata", "scores.gistic", package = "maftools")
laml.plus.gistic = read.maf(maf = laml.maf, gisticAllLesionsFile = all.lesions, gisticAmpGenesFile = amp.genes, 
  gisticDelGenesFile = del.genes, gisticScoresFile = scores.gis, isTCGA = TRUE)
laml = read.maf(maf = laml.maf, clinicalData = laml.clin)

# Primary APL MAF
primary.apl = system.file("extdata", "APL_primary.maf.gz", package = "maftools")
primary.apl = read.maf(maf = primary.apl)
# Relapse APL MAF
relapse.apl = system.file("extdata", "APL_relapse.maf.gz", package = "maftools")
relapse.apl = read.maf(maf = relapse.apl)

pt.vs.rt <- mafCompare(m1 = primary.apl, m2 = relapse.apl, m1Name = "Primary", m2Name = "Relapse", 
  minMut = 5)

laml.gistic = readGistic(gisticAllLesionsFile = all.lesions, gisticAmpGenesFile = amp.genes, 
  gisticDelGenesFile = del.genes, gisticScoresFile = scores.gis, isTCGA = TRUE)

output$maftools_fields_summary <- shiny::renderPrint({
  getFields(laml)
})

output$maftools_sample_summary <- shiny::renderDataTable({
  getSampleSummary(laml)
}, options = list(pageLength = 10, lengthMenu = list(list(5, 10, 25, 50, -1), list(5, 
  10, 25, 50, "All")), dom = "Bfrtlip", buttons = c("copy", "csv", "excel", "pdf", 
  "print")))

output$maftools_gene_summary <- shiny::renderDataTable({
  getGeneSummary(laml)
}, options = list(pageLength = 10, lengthMenu = list(list(5, 10, 25, 50, -1), list(5, 
  10, 25, 50, "All")), dom = "Bfrtlip", buttons = c("copy", "csv", "excel", "pdf", 
  "print")))
output$maftools_clinical_data <- shiny::renderDataTable({
  getClinicalData(laml)
}, options = list(pageLength = 10, lengthMenu = list(list(5, 10, 25, 50, -1), list(5, 
  10, 25, 50, "All")), dom = "Bfrtlip", buttons = c("copy", "csv", "excel", "pdf", 
  "print")))
output$maftools_plot_maf_summary <- shiny::renderPlot({
  progress$set(message = "Maftools: plotmafSummary()", value = 0.1)
  plotmafSummary(maf = laml, rmOutlier = TRUE, addStat = "median", dashboard = TRUE, 
    titvRaw = FALSE)
})

output$maftools_plot_oncoplots <- shiny::renderPlot({
  progress$set(message = "Maftools: oncoplot()", value = 0.15)
  oncoplot(maf = laml, top = 10, fontSize = 12)
})

output$maftools_plot_oncoplots_cnv <- shiny::renderPlot({
  progress$set(message = "Maftools: oncoplot() with CNV", value = 0.2)
  oncoplot(maf = laml.plus.gistic, top = 10, fontSize = 12)
})

# for maftools_plot_oncoplots_advanced
col = RColorBrewer::brewer.pal(n = 8, name = "Paired")
names(col) = c("Frame_Shift_Del", "Missense_Mutation", "Nonsense_Mutation", "Multi_Hit", 
  "Frame_Shift_Ins", "In_Frame_Ins", "Splice_Site", "In_Frame_Del")

# Color coding for FAB classification; try getAnnotations(x = laml) to see
# available annotations.
fabcolors = RColorBrewer::brewer.pal(n = 8, name = "Spectral")
names(fabcolors) = c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7")
fabcolors = list(FAB_classification = fabcolors)

# MutSig reusults
laml.mutsig <- system.file("extdata", "LAML_sig_genes.txt.gz", package = "maftools")
output$maftools_plot_oncoplots_advanced <- shiny::renderPlot({
  progress$set(message = "Maftools: oncoplot() with advanced", value = 0.25)
  oncoplot(maf = laml, colors = col, mutsig = laml.mutsig, mutsigQval = 0.01, clinicalFeatures = "FAB_classification", 
    sortByAnnotation = TRUE, annotationColor = fabcolors)
})

laml.titv = titv(maf = laml, plot = FALSE, useSyn = TRUE)
# plot titv summary
output$maftools_plot_titv <- shiny::renderPlot({
  progress$set(message = "Maftools: plotTiTv() for ransition and transversions", 
    value = 0.3)
  plotTiTv(res = laml.titv)
})

output$maftools_plot_lollipop <- shiny::renderPlot({
  progress$set(message = "Maftools: lollipopPlot2()", value = 0.4)
  lollipopPlot2(m1 = primary.apl, m2 = relapse.apl, gene = "PML", AACol1 = "amino_acid_change", 
    AACol2 = "amino_acid_change", m1_name = "Primary", m2_name = "Relapse")
})

output$maftools_plot_mutation_load <- shiny::renderPlot({
  progress$set(message = "Maftools: tcgaCompare() for mutation load", value = 0.45)
  laml.mutload = tcgaCompare(maf = laml, cohortName = "Example-LAML")
})

output$maftools_plot_vaf_box <- shiny::renderPlot({
  progress$set(message = "Maftools: plotVaf()", value = 0.5)
  plotVaf(maf = laml, vafCol = "i_TumorVAF_WU")
})

output$maftools_plot_gistic_genome <- shiny::renderPlot({
  progress$set(message = "Maftools: gisticChromPlot()", value = 0.55)
  gisticChromPlot(gistic = laml.gistic, markBands = "all")
})

output$maftools_plot_gistic_bubble <- shiny::renderPlot({
  progress$set(message = "Maftools: gisticBubblePlot()", value = 0.6)
  gisticBubblePlot(gistic = laml.gistic)
})

output$maftools_plot_gistic_oncoplot <- shiny::renderPlot({
  progress$set(message = "Maftools: gisticOncoPlot()", value = 0.65)
  gisticOncoPlot(gistic = laml.gistic, clinicalData = getClinicalData(x = laml), 
    clinicalFeatures = "FAB_classification", sortByAnnotation = TRUE, top = 10)
})

output$maftools_plot_somatic_inter <- shiny::renderPlot({
  progress$set(message = "Maftools: somaticInteractions()", value = 0.65)
  somaticInteractions(maf = laml, top = 25, pvalue = c(0.05, 0.1))
})

output$maftools_plot_somatic_inter_oncostrip <- shiny::renderPlot({
  progress$set(message = "Maftools: oncostrip()", value = 0.7)
  oncostrip(maf = laml, genes = c("TP53", "FLT3", "RUNX1"))
})

output$maftools_plot_oncodrive <- shiny::renderPlot({
  progress$set(message = "Maftools: plotOncodrive()", value = 0.75)
  laml.sig = oncodrive(maf = laml, AACol = "Protein_Change", minMut = 5, pvalMethod = "zscore")
  plotOncodrive(res = laml.sig, fdrCutOff = 0.1, useFraction = TRUE)
})

output$maftools_plot_survival <- shiny::renderPlot({
  progress$set(message = "Maftools: mafSurvival()", value = 0.75)
  laml.sig = oncodrive(maf = laml, AACol = "Protein_Change", minMut = 5, pvalMethod = "zscore")
  mafSurvival(maf = laml, genes = "DNMT3A", time = "days_to_last_followup", Status = "Overall_Survival_Status", 
    isTCGA = TRUE)
})

output$maftools_plot_clinical_enrichment <- shiny::renderPlot({
  progress$set(message = "Maftools: plotEnrichmentResults()", value = 0.8)
  fab.ce = clinicalEnrichment(maf = laml, clinicalFeature = "FAB_classification")
  plotEnrichmentResults(enrich_res = fab.ce, pVal = 0.05)
})

output$maftools_plot_clusters <- shiny::renderPlot({
  tcga.ab.2972.het = inferHeterogeneity(maf = laml, tsb = "TCGA-AB-2972", vafCol = "i_TumorVAF_WU")
  progress$set(message = "Maftools: plotClusters()", value = 1)
  plotClusters(clusters = tcga.ab.2972.het)
  progress$close()
})
