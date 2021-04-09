#' @export
PCA <- function(data){
    M <- prcomp(data, center = TRUE,scale. = TRUE)
    M
}

