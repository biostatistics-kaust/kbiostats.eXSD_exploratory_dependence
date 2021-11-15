#' Load a set of CSV files
#' @param path      Directory where the files are located
#' @param filenames List with filenames to load.
#'                  ` list(
#'                    code1="/path1",
#'                    code2="/path2",
#'                    code3="/path3"
#'                  ) `
#' @param length    Maximum number of points per file
#' @returns
#' List with file contents ordered according to the names:
#' 
#' `
#'    list(
#'      code1=matrix1,
#'      code2=matrix1,
#'      code3=matrix1
#'    )
#' `
#' @export
load_csv_datasets <- function(path, filenames, length=NULL) {
  filecodes <- names(filenames)
  read_filename <- function(fname) as.matrix(read.csv(paste0(path, "/", fname), header = TRUE, sep = ","))
  dataset_values <- list()
  for (filecode in filecodes) {
    X <- read_filename(filenames[[filecode]])
    if(!is.null(length)){
      X <- X[1:length, ]
    }
    dataset_values[[filecode]] <- X
  }
  dataset_values
}


#' Load a CSV file (split in segments)
#' @param path      .   .
#' @param range         Sliding window settings: `list(start, stop, step)`
#' @returns
#' List with the sliding windows
#' 
#' `
#'    list(
#'      matrix_segment1,
#'      matrix_segment2,
#'      matrix_segment3
#'    )
#' `
#' @export
load_partitioned_csv_dataset <- function(path, range=list(start=1, end=NULL, step=200)) {
  stopifnot(range$start > 0)
  stopifnot(range$step > 0)
  stopifnot(is.null(range$end) || range$end > range$start)
  read_filename <- function(fname) as.matrix(read.csv(fname, header = TRUE, sep = ","))
  dataset_values <- list()
  X <- read_filename(path)
  N <- nrow(X)
  if(is.null(range$end)){
    range$end <- N
  }
  n <- 1
  i <- range$start
  max_index <- range$end - range$step
  while (i <= max_index) {
    j <- i + range$step
    dataset_values[[n]] <- X[i:j, ]
    i <- j
    n <- n + 1
  }
  dataset_values
}
