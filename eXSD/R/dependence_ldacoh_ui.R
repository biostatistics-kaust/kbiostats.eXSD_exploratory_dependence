#' @export
lagged_dualfreq_coherence_controller <- function(dataparameters) {
  fs <- dataparameters$fs
  sampling_frequency <- dataparameters$fs
  mat2df <- function(m, preffix="ch.", m_colnames=NULL){
      r <- nrow(m)
      c <- ncol(m)
      rows <- (1:(r*c) - 1) %/% ncol(m) + 1
      cols <- (1:(r*c) - 1) %% ncol(m) + 1
      c_names <- if(!is.null(rownames(m))) {colnames(m)[cols]} else if (!is.null(m_colnames)) {m_colnames[cols]} else {paste0(preffix, cols)} 
      r_names <- if(!is.null(colnames(m))) {rownames(m)[rows]} else if (!is.null(m_colnames)) {m_colnames[rows]} else {paste0(preffix, rows)} 
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
      G <- as.directed(graph.adjacency(g, weighted = T))
      com <- cluster_spinglass(G, spins=5)
      w <- E(G)$weight
      w <- 5 * (w - min(w)) / (max(w) - min(w))
      #w <- w[w > quantile(w, 0.75)]
      E(G)$weight <- w
      E(G)$width <- w
      V(G)$color <- "#5cbdf1"
      G
  }

  spec2df <- function(m, preffix="ch", col="spectrum"){
      df <- list(
          w=m$freqs
      )
      m_names <- if(!is.null(colnames(m[[col]]))) paste0("freq.", colnames(m[[col]])) else paste0(preffix, 1:ncol(m[[col]]))
      for (i in 1:ncol(m[[col]])) {
         df[[m_names[i]]] <- m[[col]][, i]
      }
      df <- as.data.frame(df)
      melt(df, id="w")
  }

  networkWidget <- basicNetworkWidget(preffix="PCoh")

  server <- function(id, dataset) {
      ns <- NS(id)
      shiny::moduleServer(
          id,
          function(input, output, session){
              #
              number_freq_points <- 200
              #
              #
              output$uiBlockIndex0 <- renderUI({
                  sliderInput(session$ns("txtBlockIndex0"), "Block index:", 0, input$txtBlockNumber - 1, 0, step=1)
              })
              output$uiBlockIndex1 <- renderUI({
                  sliderInput(session$ns("txtBlockIndex1"), "Block index:", 0, input$txtBlockNumber - 1, 0, step=1)
              })
              #
              #
              data.ldac <- reactive({
                  lagged_dualfreq_coherence(
                      as.matrix(dataset()),
                      number_blocks=input$txtBlockNumber, 
                      block_index0=input$txtBlockIndex0, 
                      freqs0=input$txtFreqs0 / sampling_frequency, 
                      block_index1=input$txtBlockIndex1, 
                      freqs1=input$txtFreqs1 / sampling_frequency,
                      length_section=400
                  )
              })
              #
              output$tsSpectrum1 <- renderPlotly({
                  ggplotly(ggplot(
                      spec2df(data.ldac(), preffix="SPEC", col="spectrum1"),
                      aes(
                          x=w, 
                          y=value,
                          colour=variable,
                          group=variable
                      )
                  ) + geom_line(              
                  ) +  facet_wrap(~variable, ncol=2)
                  , width = 2.5*750, height = 2.5*130 * ncol(dataset()) )

              })
              #
              output$tsSpectrum2 <- renderPlotly({
                  ggplotly(ggplot(
                      spec2df(data.ldac(), preffix="SPEC", col="spectrum2"),
                      aes(
                          x=w, 
                          y=value,
                          colour=variable,
                          group=variable
                      )
                  ) + geom_line(              
                  ) +  facet_wrap(~variable, ncol=2)
                  , width = 2.5*750, height = 2.5*130 * ncol(dataset()))

              })
              #
              output$tsCohMatrix <- renderPlotly({
                  idx <- as.integer(ceiling(input$txtFrequency *number_freq_points)) + 1
                  data <- mat2df(data.ldac()$coh, m_colnames=colnames(dataset()))
                  ggplotly(
                  ggplot(
                      data, 
                      aes(source, destination)
                  ) + geom_tile(aes(fill = value)) + #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu"))) + #scale_y_discrete(limits = rev(unique(data_heatmap$Month))) +
                      #ggtitle("PCA") +
                      xlab(NULL) + ylab(NULL) +
                      scale_y_discrete(limits = rev(unique(data$destination))) + 
                      theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              panel.background = element_blank(),
                              axis.text.x = element_text(angle = 90))
                  , width = 500, height = 400)
              })
              
              #utput$tsCohGraph <- renderPlot({
              #    G <- mat2graph(data.ldac()$coh, m_colnames=colnames(dataset()))
              #    #plot(G, layout=layout_in_circle, vertex.size=40)
              #    plot(G, layout=layout.fruchterman.reingold, vertex.size=40, edge.arrow.size=1)
              #}, res=250, width = 3.5*700, height = 3.5*500)

              networkWidget$server(input, output, getMatrixFunctor=(function(){
                Q <- data.ldac()$coh
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
                  title = "Block settings", status = "warning", solidHeader = TRUE, width=12,
                  collapsible = TRUE, collapsed=TRUE,
                  #tags$p("Coherence is estimated the spectrum through  a Vector Autoregressive Model:"),
                  sliderInput(ns("txtBlockNumber"), "Number of blocks:", 2, 20, 2, step=1)
              ),
              
              box(
                  title = "Channel A", status = NULL, solidHeader = FALSE, width=6, collapsible = FALSE,
                  uiOutput(ns("uiBlockIndex0")),
                  sliderInput(ns("txtFreqs0"), "Frequency range [Hz]:", 0, sampling_frequency/2, value=c(0, 20), step=1)
              ),
              box(
                  title = "Channel B", status = NULL, solidHeader = FALSE, width=6, collapsible = FALSE,
                  uiOutput(ns("uiBlockIndex1")),
                  sliderInput(ns("txtFreqs1"), "Frequency range [Hz]:", 0, sampling_frequency/2, value=c(0, 20), step=1)
              ),
              
              box(
                  title = "Bivariate coherence", status = "primary", solidHeader = TRUE, width=6,
                  collapsible = TRUE,
                  plotlyOutput(ns("tsCohMatrix"))
              ),

              
              box(
                  title = "Coherence graph", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  #plotOutput(ns("tsCohGraph"))#,
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




