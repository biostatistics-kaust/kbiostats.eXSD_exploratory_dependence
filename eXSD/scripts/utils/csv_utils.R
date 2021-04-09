#' @export
load_csv_datasets <- function(path, filenames, length=NULL) {
  filecodes <- names(filenames)
  #
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
