
default_colnames <- function(X, fmt="ch%02d", force_name=FALSE){
  cols <- 1: ncol(X)
  c_names <- if(!force_name && !is.null(colnames(X))) colnames(X) else sprintf(fmt, cols)
  c_names
}

default_rownames <- function(X, fmt="ch%02d", force_name=FALSE){
  rows <- 1: nrow(X)
  r_names <- if(!force_name && !is.null(rownames(X))) rownames(X) else sprintf(fmt, rows)
  r_names
}

#' @export
add_colnames_to_matrix <- function(X, fmt="ch%02d", force_name=FALSE){
  names <- default_colnames(X, fmt, force_name)
  #colnames(X) <- default_colnames(X, fmt, force_name)
  colnames(X) <- names
  #rownames(X) <- default_rownames(X, fmt, force_name)
  X
}

#' @export
add_names_to_matrix <- function(X, fmt="ch%02d", force_name=FALSE){
  stopifnot(ncol(X) == nrow(X))
  names <- default_colnames(X, fmt, force_name)
  #colnames(X) <- default_colnames(X, fmt, force_name)
  colnames(X) <- names
  rownames(X) <- names
  #rownames(X) <- default_rownames(X, fmt, force_name)
  X
}

#' @export
matrix_as_dataframe <- function(X, fmt="ch%02d", force_name=FALSE){
  r <- nrow(X)
  c <- ncol(X)
  rows <- (1:(r*c) - 1) %/% c + 1
  cols <- (1:(r*c) - 1) %% c + 1
  c_names <- default_colnames(X, fmt, force_name)[cols]
  r_names <- default_rownames(X, fmt, force_name)[rows]
  df <- list(
    source=factor(r_names),
    destination=factor(c_names),
    #feature=factor(names),
    value=as.vector(X)
  )
  df
}

#' @export
matrix_as_feature <- function(X, fmt="ch%02d", force_name=FALSE){
  r <- nrow(X)
  c <- ncol(X)
  rows <- (1:(r*c) - 1) %/% c + 1
  cols <- (1:(r*c) - 1) %% c + 1
  c_names <- default_colnames(X, fmt, force_name)[cols]
  r_names <- default_rownames(X, fmt, force_name)[rows]
  names <- paste0(r_names, "_", c_names)
  df <- list(
    feature=factor(names),
    value=as.vector(X)
  )
  df
}

#' @export
matrix_list_as_feature <- function(X, lstfmt="lst%03d", matfmt="ch%02d", force_name=FALSE){
  n <- length(X)
  if(n == 0){
    return(list(feature=c(), value=c()))
  }
  r <- nrow(X[[1]])
  c <- ncol(X[[1]])
  rows <- (1:(r*c) - 1) %/% c + 1
  cols <- (1:(r*c) - 1) %% c + 1
  feature <- c()
  value <- c()
  for(idx in 1:n) {
    ### colfmt <- sprintf("%s_%s", sprintf(lstfmt, idx), matfmt)
    lst_name <- sprintf(lstfmt, idx)
    c_names <- default_colnames(X[[1]], matfmt, force_name)[cols]
    r_names <- default_rownames(X[[1]], matfmt, force_name)[rows]
    names <- paste0(lst_name, "_", r_names, "_", c_names)
    feature <- c(feature, names)
    value <- c(value, as.vector(X[[idx]]))
  }
  df <- list(
    feature=feature,
    value=value
  )
  df
}
