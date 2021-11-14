#' @export
partial_directed_coherence_controller <- function(dataparameters) {
  fs <- dataparameters$fs
  ts2df_dualcol <- function(m, i, j){
      df <- list(
          x=1:nrow(m)
      )
      c_names <- if(!is.null(colnames(m))) colnames(m) else paste0(preffix, 1:ncol(m))
      df[[c_names[i]]] <- m[, i]
      df[[c_names[j]]] <- m[, j]
      df <- as.data.frame(df)
      melt(df, id="x")
  }

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

  mat2df <- function(m, preffix="ch.", m_colnames=NULL){
      r <- nrow(m)
      c <- ncol(m)
      rows <- (1:(r*c) - 1) %/% ncol(m) + 1
      cols <- (1:(r*c) - 1) %% ncol(m) + 1
      c_names <- if(!is.null(colnames(m))) {colnames(m)[cols]} else if (!is.null(m_colnames)) {m_colnames[cols]} else {paste0(preffix, cols)} 
      r_names <- if(!is.null(rownames(m))) {rownames(m)[rows]} else if (!is.null(m_colnames)) {m_colnames[rows]} else {paste0(preffix, rows)} 
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
      E(G)$weight <- w * 100
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
                  partial_directed_coherence(
                      dataset(),
                      var_order=input$txtVarOrder,
                      var_estimation_type=input$lstVarEstimation,
                      number_freq_points=number_freq_points,
                      type=input$radPDCType)
              })
              output$objSrc <- renderUI({
                  #sliderInput(session$ns("txtSrc"), "Input channel:", 1, ncol(dataset()), 1, step=1)
          ds_columns <- colnames(dataset())
          choice_values <- 1:length(ds_columns)
          names(choice_values) <- ds_columns
          selectInput(session$ns("txtSrc"), "Source channel A:", choices=choice_values, selected=1)
              })
              output$objDst <- renderUI({
                  #sliderInput(session$ns("txtDst"), "Output channel:", 1, ncol(dataset()), 2, step=1)
          ds_columns <- colnames(dataset())
          choice_values <- 1:length(ds_columns)
          names(choice_values) <- ds_columns
          selectInput(session$ns("txtDst"), "Destination channel B:", choices=choice_values, selected=2)
              })
              
              output$tsCoh <- renderPlot({
                  i <- as.integer(input$txtSrc)
                  j <- as.integer(input$txtDst)
                  ggplot(
                      data.frame(
                          frequencies=metric()$freqs * fs,
                          partial_directed_coherence=get_marginal_coh(metric()$coh, i, j)
                      ),
                      aes(
                          x=frequencies, 
                          y=partial_directed_coherence
                      )
                  ) + geom_line()

              }, res=250, width = 2.5*750, height = 2.5*300)

              output$tsCohMatrix <- renderPlot({
                  idx <- as.integer(ceiling(input$txtFrequency / fs * number_freq_points)) + 1
                  data <- mat2df(metric()$coh[[idx]], m_colnames=colnames(dataset()))
                  ggplot(
                      data, 
                      aes(source, destination)
                  ) + geom_tile(aes(fill = value)) + #scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu"))) + #scale_y_discrete(limits = rev(unique(data_heatmap$Month))) +
                      #ggtitle("PCA") +
                      #geom_text(aes(label=round(value, 1))) + 
                      xlab(NULL) + ylab(NULL) +
                      scale_y_discrete(limits = rev(unique(data$destination))) + 
                      #scale_fill_gradient(low = "#132B43", high = "#56B1F7", na.value = NA, limits=c(0, 1)) +
                      theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              panel.background = element_blank(),
                              axis.text.x = element_text(angle = 90))
              }, res=250, width = 2.5*700, height = 2.5*500)

              #output$tsPlot <- renderPlot({
              #    i <- as.integer(input$txtSrc)
              #    j <- as.integer(input$txtDst)
              #    df <- ts2df_dualcol(dataset(), i, j)
              #    ggplot(
              #        df,
              #        aes(
              #            x=x, 
               #           y=value,
              #            colour=variable,
              #            group=variable
              #        )
              #    ) + geom_line(                    
              #    ) +  facet_wrap(~variable, ncol=2)
              #}, res=250, width = 2.5*750, height = 2.5*300)
              
              #output$tsCohGraph <- renderPlot({
              #    idx <- as.integer(ceiling(input$txtFrequencyGraph / fs * number_freq_points)) + 1
              #    G <- mat2graph(metric()$coh[[idx]], m_colnames=colnames(dataset()))
              #    #plot(G, layout=layout_in_circle, vertex.size=40)
              #    plot(G, layout=layout.fruchterman.reingold, vertex.size=40, edge.arrow.size=1)
              #}, res=250, width = 3.5*700, height = 3.5*500)

              networkWidget$server(input, output, getMatrixFunctor=(function(){
                idx <- as.integer(ceiling(input$txtFrequencyGraph / fs * number_freq_points)) + 1
                Q <- metric()$coh[[idx]]
                colnames(Q) <- colnames(dataset())
                Q
              }))
          }
      )
  }

  client <- function(id) {
      ns <- NS(id)
      ui_tags <- tags$div(
          fluidRow(
        box(
                  title = "Spectrum estimation parameters", status = "warning", solidHeader = TRUE, width=12,
                  collapsible = TRUE, collapsed=TRUE,
                  #tags$p("Coherence is estimated the spectrum through  a Vector Autoregressive Model:"),
                  helpText('Coherence is estimated the spectrum through  a Vector Autoregressive Model:
                      $$
                          Y\\left(t\\right)=\\sum_{\\ell=1}^{p}\\Phi_{\\ell} X\\left(t-\\ell\\right)+\\varepsilon\\left(t\\right)
                      $$
                      $$
                          \\varepsilon\\left(t\\right)
                          \\sim
                          \\mathcal{N}\\left(0,\\Sigma\\right)
                      $$
                  '),
                  sliderInput(ns("txtVarOrder"), "VAR order:", 1, 10, 2, step=1),
                  ##radioButtons(ns("lstVarEstimation"), "VAR estimation method", choiceNames=c( 'OLS', 'LASSLE (LASSO + LSE)', 'LASSO (BigVAR package)'), choiceValues=c("RawLSE", "LASSLE", "LassoBigVAR"))
                  radioButtons(ns("lstVarEstimation"), "Estimation method", choiceNames=c( 'Parametric estimator: VAR/OLS', 'Parametric estimator: VAR/LASSLE (LASSO + LSE)', 'Parametric estimator: VAR/LASSO (BigVAR package)'), choiceValues=c("RawLSE", "LASSLE", "LassoBigVAR"))
                  #checkboxInput(ns('chkSparcity'), 'Assume sparcity in \\( \\{\\Phi_{\\ell}\\} \\)', FALSE)
                  #sliderInput(ns("txtDst"), "Output channel:", 1, 10, 0, step=1),
                  #textInput(ns("text"), "Text input:")
              ),
              box(
                  title = "PDC parameters", status = "primary", solidHeader = TRUE, width=12,
                  collapsible = TRUE,
                  radioButtons(ns("radPDCType"), "PDC Type", choiceNames=c('Normal PDC', 'Generalized PDC'), choiceValues=c("PDC", "GPDC"))
              ),
              box(
                  title = "Bivariate PDC", status = "primary", solidHeader = TRUE, width=6,
                  collapsible = TRUE,
                  uiOutput(ns("objSrc")),
                  uiOutput(ns("objDst")),
                  #sliderInput(ns("txtSrc"), "Input channel:", 1, 10, 0, step=1),
                  #sliderInput(ns("txtDst"), "Output channel:", 1, 10, 0, step=1),
                  #textInput(ns("text"), "Text input:")
                  plotOutput(ns("tsCoh")),
                  #helpText("Channels:"),
                  #plotOutput(ns("tsPlot")),
              ),
              box(
                  title = "PDC matrix", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  sliderInput(ns("txtFrequency"), "Frequency:", 0, 0.5*fs, 0, step=0.01),
                  plotOutput(ns("tsCohMatrix"))
              ),
              box(
                  title = "PDC graph", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  sliderInput(ns("txtFrequencyGraph"), "Frequency:", 0, 0.5*fs, 0, step=0.01),
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




