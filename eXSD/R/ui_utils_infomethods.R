defineMethod <- function(id, label, controller, icon=shiny::icon("calculator"), color="aqua") {
    list(id=id, label=label, server=controller$server, client=controller$client, icon=icon, color=color)
}

# infomethod XD
collect_methods <- function(dataparameters) {
  list(
    defineMethod("corr", "Correlation", correlation_controller(dataparameters), shiny::icon("arrows"), "aqua"),
    defineMethod("coh", "Coherence", coherence_controller(dataparameters), shiny::icon("arrows-alt"), "aqua"),
    defineMethod("pcoh", "Partial Coherence", partial_coherence_controller(dataparameters), shiny::icon("arrows-alt"), "aqua"),
    defineMethod("pdc", "Partial Directed Coherence", partial_directed_coherence_controller(dataparameters), shiny::icon("exchange"), "orange"),
    defineMethod("ldac", "Lagged dual-frequency auto-coherence", lagged_dualfreq_coherence_controller(dataparameters), shiny::icon("reply-all"), "orange"),
    defineMethod("pca", "Principal Component Analysis", pca_controller(dataparameters), shiny::icon("exchange"), "aqua"),
    defineMethod("spca", "Spectral Principal Component Analysis", spectral_pca_controller(dataparameters), shiny::icon("exchange"), "aqua"),
    defineMethod("dgpca", "Generalized Dynamic Principal Component Analysis", generalized_dynamic_pca_controller(dataparameters), shiny::icon("th"), "teal")
  )
  
  #defineMethod("hcc", "Hierarchical Cluster Coherence", NULL, shiny::icon("th"), "orange"),
  #defineMethod("gse", "Generalized Shrinkage Estimator", NULL, shiny::icon("refresh"), "teal"),
  #defineMethod("lassepdc", "LASSLE Partial Directed Coherence", NULL, shiny::icon("exchange"), "orange"),
  #defineMethod("scau", "Spectral Causality", NULL, shiny::icon("random"), "red")
}