defineMethod <- function(id, label, controller, icon=icon("calculator"), color="aqua") {
    list(id=id, label=label, server=controller$server, client=controller$client, icon=icon, color=color)
}

# infomethod XD
collect_methods <- function() {
  list(
    defineMethod("corr", "Correlation", correlation_controller(), icon("arrows"), "aqua"),
    defineMethod("coh", "Coherence", coherence_controller(), icon("arrows-alt"), "aqua"),
    defineMethod("pcoh", "Partial Coherence", partial_coherence_controller(), icon("arrows-alt"), "aqua"),
    defineMethod("pdc", "Partial Directed Coherence", partial_directed_coherence_controller(), icon("exchange"), "orange"),
    defineMethod("ldac", "Lagged dual-frequency auto-coherence", lagged_dualfreq_coherence_controller(), icon("reply-all"), "orange"),
    defineMethod("pca", "Principal Component Analysis", pca_controller(), icon("exchange"), "aqua"),
    defineMethod("spca", "Spectral Principal Component Analysis", spectral_pca_controller(), icon("exchange"), "aqua"),
    defineMethod("dgpca", "Generalized Dynamic Principal Component Analysis", generalized_dynamic_pca_controller(), icon("th"), "teal")
  )
  #defineMethod("hcc", "Hierarchical Cluster Coherence", NULL, icon("th"), "orange"),
  #defineMethod("gse", "Generalized Shrinkage Estimator", NULL, icon("refresh"), "teal"),
  #defineMethod("lassepdc", "LASSLE Partial Directed Coherence", NULL, icon("exchange"), "orange"),
  #defineMethod("scau", "Spectral Causality", NULL, icon("random"), "red")
}