#' @export
pca_controller <- function(dataparameters) {
  fs <- dataparameters$fs
  ts2df <- function(m, preffix="ch"){
      df <- list(
          x=1:nrow(m)
      )
      m_names <- if(!is.null(colnames(m))) paste0("ts.", colnames(m)) else paste0(preffix, 1:ncol(m))
      for (i in 1:ncol(m)) {
         df[[m_names[i]]] <- m[, i]
      }
      df <- as.data.frame(df)
      melt(df, id="x")
  }

  server <- function(id, dataset) {
      shiny::moduleServer(
          id,
          function(input, output, session){
              data.pca <- reactive({PCA(dataset())})
              output$tsPlot <- renderPlot({
                  ggplot(
                      ts2df(dataset()),
                      aes(
                          x=x, 
                          y=value,
                          colour=variable,
                          group=variable
                      )
                  ) + geom_line(              
                  ) +  facet_wrap(~variable, ncol=2)

              }, res=250, width = 2.5*750, height = 2.5*130 * ncol(dataset()))
              output$tsPCA <- renderPlot({
                  ggplot(
                      ts2df(data.pca()$x, preffix="PC"),
                      aes(
                          x=x, 
                          y=value,
                          colour=variable,
                          group=variable
                      )
                  ) + geom_line(              
                  ) +  facet_wrap(~variable, ncol=2)

              }, res=250, width = 2.5*750, height = 2.5*130 * ncol(dataset()))
              output$PCs <- renderPlot({
                  ggbiplot(data.pca(),
                      obs.scale = 1, 
                      var.scale=1,
                      ellipse=T,
                      circle=F,
                      varname.size=3,
                      var.axes=T,
                      #groups=iris$Species, #no need for coloring, I'm making the points invisible
                      alpha=0)
              }, res=250, width = 2.5*700, height = 2.5*500)
          }
      )
  }

  client <- function(id) {
      ns <- NS(id)
      ui_tags <- tags$div(
          fluidRow(
              box(
                  title = "Principal components (2)", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(ns("PCs"))
              )
          ),
          fluidRow(
              box(
                  title = "Input time series", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(ns("tsPlot"))
              ),
              box(
                  title = "PCA", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(ns("tsPCA"))
              )
          )
      )
      ui_tags
  }

  list(
    client=client,
    server=server
  )
}




