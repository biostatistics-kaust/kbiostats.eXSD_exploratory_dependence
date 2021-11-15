#' Correlation
#' 
#' @param data    Input matrix with proper column names.
#' @return
#' Correlation matrix with column names inherited from the data
#' 
correlation <- function(data){
  #data <- 
  add_colnames_to_matrix(data, fmt="ch%02d", force_name=FALSE)
  M <- cor(data)
  add_names_to_matrix(M, fmt="ch%02d", force_name=FALSE)
  M
}

#' @export
correlation_as_feature <- function(X, fmt="corr_ch%02d", force_name=FALSE){
  Y <- correlation(X)
  matrix_as_feature(Y, fmt, force_name)
}


