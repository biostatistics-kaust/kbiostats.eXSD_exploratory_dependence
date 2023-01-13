#' @export
correlation_controller <- function(fs) {

  ts2df <- function(m, preffix="ch."){
      df <- list(
          x=1:nrow(m)
      )
      c_names <- if(!is.null(colnames(m))) colnames(m) else paste0(preffix, 1:ncol(m))
      for (i in 1:ncol(m)) {
         df[[c_names[i]]] <- m[, i]
      }
      df <- as.data.frame(df)
      melt(df, id="x")
  }


  mat2df <- function(m, preffix="ch."){
      r <- nrow(m)
      c <- ncol(m)
      rows <- (1:(r*c) - 1) %/% ncol(m) + 1
      cols <- (1:(r*c) - 1) %% ncol(m) + 1
      c_names <- if(!is.null(colnames(m))) rownames(m)[cols] else paste0(preffix, cols)
      r_names <- if(!is.null(rownames(m))) colnames(m)[rows] else paste0(preffix, rows)
      df <- list(
          source=factor(r_names),
          destination=factor(c_names),
          value=as.vector(m)
      )
      as.data.frame(df)
  }

  mat2graph <- function(m, preffix="ch.", m_colnames=NULL){
      g <- as.matrix(m)
      g <- g - diag(diag(g))
      colnames(g) <- m_colnames
      rownames(g) <- m_colnames
      G <- as.undirected(graph.adjacency(g, weighted = T))
      com <- cluster_spinglass(G, spins=5)
      w <- E(G)$weight
      w <- 5 * (w - min(w)) / (max(w) - min(w))
      #w <- w[w > quantile(w, 0.75)]
      E(G)$weight <- w
      E(G)$width <- w
      V(G)$color <- "#5cbdf1"
      G
  }

  networkWidget <- basicNetworkWidget(preffix="PCoh")

  server <- function(id, dataset) {
      shiny::moduleServer(
          id,
          function(input, output, session){
              metric <- reactive({correlation(dataset())})
              output$tsPlot <- renderPlotly({
                  ggplotly(
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
                  , width = 500, height = 100 * ncol(dataset()))
              })#, res=250, width = 2.5*750, height = 2.5*130 * ncol(dataset()))
              output$tsCorrMatrix <- renderPlotly({#
                  data <- mat2df(metric())
                  ggplotly(
                  ggplot(
                      data, 
                      aes(source, destination)
                  ) + geom_raster(aes(fill = value)) + #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu"))) + #scale_y_discrete(limits = rev(unique(data_heatmap$Month))) +
                      #ggtitle("PCA") +
                      scale_y_discrete(limits = rev(unique(data$destination))) + 
                      theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              panel.background = element_blank(),
                              axis.text.x = element_text(angle = 90))
                  , width = 500, height = 400)
              })#, res=250, width = 2.5*700, height = 2.5*500)

              #output$tsCorGraph <- renderPlot({
              #    G <- mat2graph(metric(), m_colnames=colnames(dataset()))
              #    #plot(G, layout=layout_in_circle, vertex.size=40)
              #    plot(G, layout=layout.fruchterman.reingold, vertex.size=40, edge.arrow.size=1)
              #}, res=250, width = 3.5*700, height = 3.5*500)
              
              networkWidget$server(input, output, getMatrixFunctor=(function(){
                Q <- metric()
                colnames(Q) <- colnames(dataset())
                Q
              }), is_directed=FALSE)
          }
      )
  }

  client <- function(id) {
      ns <- NS(id)
      ui_tags <- tags$div(
          fluidRow(
              box(
                  title = "Input time series", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput(ns("tsPlot"))
              ),
              box(
                  title = "Correlation", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput(ns("tsCorrMatrix"))
              ),
              box(
                  title = "Correlation graph", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  #plotOutput(ns("tsCorGraph"))#,
                  networkWidget$client(ns)
                  #helpText("Edges with magnitudes in the upper quartile")
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




