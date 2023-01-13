#' @export
partial_coherence_controller <- function(dataparameters) {
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
                partial_coherence(
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
                ds_columns <- colnames(dataset())
                choice_values <- 1:length(ds_columns)
                names(choice_values) <- ds_columns
                selectInput(session$ns("txtSrc"), "Channel A:", choices=choice_values, selected=1)
              })
              output$objDst <- renderUI({
                ds_columns <- colnames(dataset())
                choice_values <- 1:length(ds_columns)
                names(choice_values) <- ds_columns
                selectInput(session$ns("txtDst"), "Channel B:", choices=choice_values, selected=2)
              })
              
              output$tsCoh <- renderPlotly({
                  i <- as.integer(input$txtSrc)
                  j <- as.integer(input$txtDst)
                  ggplotly(ggplot(
                      data.frame(
                          Frequency=metric()$freqs * fs,
                          Coherence=get_marginal_coh(metric()$coh, i, j)
                      ),
                      aes(
                          x=Frequency, 
                          y=Coherence
                      )
                  ) + geom_line()
                  , width = 500, height = 300)

              })

              output$tsCohMatrix <- renderPlotly({
                  idx <- as.integer(ceiling(input$txtFrequency / fs * number_freq_points)) + 1
                  data <- mat2df(metric()$coh[[idx]], m_colnames=colnames(dataset()))
                  ggplotly(ggplot(
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
                  title = "Bivariate partial coherence", status = "primary", solidHeader = TRUE, width=6,
                  collapsible = TRUE,
                  uiOutput(ns("objSrc")),
                  uiOutput(ns("objDst")),
                  #sliderInput(ns("txtSrc"), "Input channel:", 1, 10, 0, step=1),
                  #sliderInput(ns("txtDst"), "Output channel:", 1, 10, 0, step=1),
                  #textInput(ns("text"), "Text input:")
                  plotlyOutput(ns("tsCoh"))
              ),
              box(
                  title = "Partial coherence matrix", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  sliderInput(ns("txtFrequency"), "Frequency:", 0, 0.5*fs, 0, step=0.01),
                  plotlyOutput(ns("tsCohMatrix"))
              ),
              box(
                  title = "Partial coherence graph", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  sliderInput(ns("txtFrequencyGraph"), "Frequency:", 0, 0.5*fs, 0, step=0.01),
                  networkWidget$client(ns)
                  #helpText("Edges with magnitudes in the upper quartile")
              ),
              
          )
      )
      ui_tags
  }


  list(
      client=client,
      server=server
  )
}

