#' @export
coherence_controller <- function(dataparameters) {
  fs <- dataparameters$fs
  mat2df <- function(m, preffix="ch.", m_colnames=NULL){
      r <- nrow(m)
      c <- ncol(m)
      rows <- (1:(r*c) - 1) %/% ncol(m) + 1
      cols <- (1:(r*c) - 1) %% ncol(m) + 1
      r_names <- if(!is.null(colnames(m))) {colnames(m)[cols]} else if (!is.null(m_colnames)) {m_colnames[cols]} else {paste0(preffix, cols)} 
      c_names <- if(!is.null(rownames(m))) {rownames(m)[rows]} else if (!is.null(m_colnames)) {m_colnames[rows]} else {paste0(preffix, rows)} 
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
      #G <- as.directed(graph.adjacency(g, weighted = T))
      G <- (graph.adjacency(g, weighted = T))
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
      ns <- NS(id)
      shiny::moduleServer(
          id,
          function(input, output, session){
        
              get_marginal_coh <- function(coh, i, j) sapply(1:length(coh), function(p) coh[[p]][i, j])
              #data <- reactiveValues(dataset = dataset)
              #reactive_dataset <- reactiveVal(dataset)
              #
              number_freq_points <- 200
              metric <- reactive({
                spectrum_info <- get_spectrum_params(input)
                #print(spectrum_info)
                coherence(
                  dataset(), 
                  number_freq_points=number_freq_points, 
                  estimation_type=spectrum_info$estimation_method, 
                  var_order=spectrum_info$VAR_order, 
                  variance_window_C_T=spectrum_info$variance_window, 
                  n_trials=spectrum_info$n_trials, 
                  win_spans=spectrum_info$spans, 
                  win=spectrum_info$win
                )
              })
              output$objSrc <- renderUI({
                  #sliderInput(session$ns("txtSrc"), "Channel A:", 1, ncol(dataset()), 1, step=1)
                  ds_columns <- colnames(dataset())
                  choice_values <- 1:length(ds_columns)
                  names(choice_values) <- ds_columns
                  selectInput(session$ns("txtSrc"), "Channel A:", choices=choice_values, selected=1)
              })
              output$objDst <- renderUI({
                #sliderInput(session$ns("txtDst"), "Channel B:", 1, ncol(dataset()), 2, step=1)
                ds_columns <- colnames(dataset())
                choice_values <- 1:length(ds_columns)
                names(choice_values) <- ds_columns
                selectInput(session$ns("txtDst"), "Channel B:", choices=choice_values, selected=2)
              })
              
              output$tsCoh <- renderPlot({
                  i <- as.integer(input$txtSrc)
                  j <- as.integer(input$txtDst)
                  v = metric()$freqs * fs
                  w = Coherence=get_marginal_coh(metric()$coh, i, j)
                  ggplot(
                      data.frame(
                          Frequency=metric()$freqs * fs,
                          Coherence=get_marginal_coh(metric()$coh, i, j)
                      ),
                      aes(
                          x=Frequency, 
                          y=Coherence
                      )
                  ) + geom_line()

              }, res=250, width = 2.5*700, height = 2.5*300)

              output$tsCohMatrix <- renderPlot({
                  idx <- as.integer(ceiling(input$txtFrequency / fs * number_freq_points)) + 1
                  data <- mat2df(metric()$coh[[idx]], m_colnames=colnames(dataset()))
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
              }, res=250, width = 2.5*700, height = 2.5*500)

              #output$tsCohGraph <- renderPlot({
              #    idx <- as.integer(ceiling(input$txtFrequencyGraph / fs * number_freq_points)) + 1
              #    G <- mat2graph(metric()$coh[[idx]], m_colnames=colnames(dataset()))
              #    #plot(G, layout=layout_in_circle, vertex.size=40)
              #    #plot(G, layout=layout.fruchterman.reingold, vertex.size=40, edge.arrow.size=)
              #    plot(G, layout=layout.fruchterman.reingold, vertex.size=40, edge.arrow.size=0)
              #}, res=250, width = 3.5*700, height = 3.5*500)
              
              networkWidget$server(input, output, getMatrixFunctor=(function(){
                idx <- as.integer(ceiling(input$txtFrequencyGraph / fs * number_freq_points)) + 1
                Q <- metric()$coh[[idx]]
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
        spectral_estimation_ui(id),
              box(
                  title = "Bivariate coherence", status = "primary", solidHeader = TRUE, width=6,
                  collapsible = TRUE,
                  uiOutput(ns("objSrc")),
                  uiOutput(ns("objDst")),
                  #sliderInput(ns("txtSrc"), "Input channel:", 1, 10, 0, step=1),
                  #sliderInput(ns("txtDst"), "Output channel:", 1, 10, 0, step=1),
                  #textInput(ns("text"), "Text input:")
                  plotOutput(ns("tsCoh"))
              ),
              box(
                  title = "Coherence matrix", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  sliderInput(ns("txtFrequency"), "Frequency:", 0, 0.5 * fs, 0, step=0.01),
                  plotOutput(ns("tsCohMatrix"))
              ),
              box(
                  title = "Coherence graph", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  sliderInput(ns("txtFrequencyGraph"), "Frequency:", 0, 0.5 * fs, 0, step=0.01),
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




