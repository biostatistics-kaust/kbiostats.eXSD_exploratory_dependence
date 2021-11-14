generalized_dynamic_pca <- function(data){
    suppressWarnings({
      data <- as.matrix(data)
      L <- tryCatch({
        #invisible(auto.gdpc(data, normalize=2, niter_max=10, expl_var=0.999, ncores=1))
        invisible(auto.gdpc(data, normalize=3, niter_max=10, expl_var=0.7, ncores=1))
      }, error = function(err){
        NULL
      })
      PCs <- if(!is.null(L)) sapply(1:length(L), function(ell) L[[ell]]$f) else NULL
      M <- prcomp(data, center = TRUE,scale. = TRUE)
      return(list(
          t=1:nrow(data),
          time_series=data,
          PCs=PCs,
          pca=M
      ))
    })
}

generalized_dynamic_pca <- function(data){
    data <- as.matrix(data)
    print(head(data))
    capture.output({
      L <- auto.gdpc(data, normalize=3, niter_max=10, expl_var=0.7, ncores=1)
    })
    PCs <- if(!is.null(L)) sapply(1:length(L), function(ell) L[[ell]]$f) else NULL
    M <- prcomp(data, center = TRUE,scale. = TRUE)
    return(list(
        t=1:nrow(data),
        time_series=data,
        PCs=PCs,
        pca=M
    ))
}

