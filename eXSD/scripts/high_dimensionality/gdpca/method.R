generalized_dynamic_pca <- function(data){
    data <- as.matrix(data)
    L <- auto.gdpc(data, normalize=2, niter_max=10, expl_var=0.999, ncores=1)
    PCs <- sapply(1:length(L), function(ell) L[[ell]]$f)
    M <- prcomp(data, center = TRUE,scale. = TRUE)
    list(
        t=1:nrow(data),
        time_series=data,
        PCs=PCs,
        pca=M
    )
}

