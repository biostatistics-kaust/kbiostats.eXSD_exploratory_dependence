#' @export
server_widget <- function(datasources) {
  infoMethods <- collect_methods()
  server <- shinyServer(function(input, output) {
   random_data <- matrix(rnorm(1000*6), ncol=6, nrow=1000)
   random_data <- as.data.frame(random_data)
   colnames(random_data) <- paste0("ch", 1:ncol(random_data))
   reactive_dataset <- reactive({ datasources[[input$chkDataSource]] })
   observe({
    for (i in 1:length(infoMethods)) {
      infoMethod <- infoMethods[[i]]
      if(!is.null(infoMethod$server)) {
        infoMethod$server(infoMethod$id, dataset=reactive_dataset)
      }
    }
   })   
  })
}