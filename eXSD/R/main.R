#' eXSD Dependency Explorer
#' 
#' @description
#'
#' \figure{logo.png}{options: width="100px" alt="eXSD Dependency Explorer"}
#' 
#' Start the main visual interface of the dependency explorer.
#' 
#' @param datasets              Named list of matrices
#' `
#'    list(
#'      code1=matrix1,
#'      code2=matrix2,
#'      code3=matrix3
#'    )
#' `
#' @param labels                Named list of labels to show in the UI
#' `
#'    list(
#'      code1="label 1",
#'      code2="label 2",
#'      code3="label 3"
#'    )
#' `
#' @param sampling_frequency    Sampling rate at the datasets were collected.
#' @param port                  Port when the RShiny app will open
#' @param launch.browser        Open browser. Default: TRUE.
#' 
#' @export
dependency.explorer <- function(datasets, labels, fs=1, port=NULL, launch.browser=TRUE) {
  stopifnot(length(datasets) == length(labels))
  stopifnot(all(names(datasets) == names(labels)))
  dataparameters <- list(fs=fs)
  shiny::runApp(list(
      ui=ui_widget(datasets, labels, dataparameters),
      server=server_widget(datasets, dataparameters)
    ),
    port=port,
    test.mode=TRUE,
    launch.browser=launch.browser
  )
  if(!launch.browser){
    print(paste("Open browser at port", port))
  }
}



