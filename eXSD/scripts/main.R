#' @export
dependency.explorer <- function(datasets, labels, port=9854) {
  stopifnot(length(datasets) == length(labels))
  stopifnot(all(names(datasets) == names(labels)))
  shiny::runApp(list(
      ui=ui_widget(datasets, labels),
      server=server_widget(datasets)
    ),
    port=port,
    test.mode=TRUE,
    launch.browser=TRUE
  )
}



