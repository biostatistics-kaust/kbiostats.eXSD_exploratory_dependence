#' @export
spectral_pca_controller <- function(dataparameters) {
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

  spec2df <- function(m, fs, preffix="ch", col="spectrum"){
      df <- list(
          Frequency=m$freqs * fs
      )
      m_names <- if(!is.null(colnames(m[[col]]))) paste0("ts.", colnames(m[[col]])) else paste0(preffix, 1:ncol(m[[col]]))
      for (i in 1:ncol(m[[col]])) {
         df[[m_names[i]]] <- m[[col]][, i]
      }
      df <- as.data.frame(df)
      melt(df, id="Frequency" , value.name="Spectrum")
  }


  server <- function(id, dataset) {
      shiny::moduleServer(
          id,
          function(input, output, session){
              data.pca <- reactive({
          spectrum_info <- get_spectrum_params(input)
          #print(spectrum_info)
          spectral_pca(
            dataset(), 
            estimation_type=spectrum_info$estimation_method, 
            var_order=spectrum_info$VAR_order, 
            variance_window_C_T=spectrum_info$variance_window, 
            n_trials=spectrum_info$n_trials, 
            win_spans=spectrum_info$spans, 
            win=spectrum_info$win)
        })
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
              
              output$tsSpectrum <- renderPlot({
                  ggplot(
                      spec2df(data.pca(), fs, preffix="SPEC", col="spectra"),
                      aes(
                          x=Frequency, 
                          y=Spectrum,
                          colour=variable,
                          group=variable
                      )
                  ) + geom_line(              
                  ) +  facet_wrap(~variable, ncol=2)

              }, res=250, width = 2.5*750, height = 2.5*130 * ncol(dataset()))
              
              output$tsPCA <- renderPlot({
                  ggplot(
                      spec2df(data.pca(), fs, preffix="PC", col="PCs"),
                      aes(
                          x=Frequency, 
                          y=Spectrum,
                          colour=variable,
                          group=variable
                      )
                  ) + geom_line(              
                  ) +  facet_wrap(~variable, ncol=2)

              }, res=250, width = 2.5*750, height = 2.5*130 * ncol(dataset()))
              
        # Biplot isn't defined for SPCA
              #output$PCs <- renderPlot({
              #    ggbiplot(data.pca()$pca,
              #        obs.scale = 1, 
              #        var.scale=1,
              #        ellipse=T,
              #        circle=F,
              #        varname.size=3,
              #        var.axes=T,
              #        #groups=iris$Species, #no need for coloring, I'm making the points invisible
              #        alpha=0)
              #}, res=250, width = 2.5*700, height = 2.5*500)
          }
      )
  }

  client <- function(id) {
      ns <- NS(id)
      ui_tags <- tags$div(
          fluidRow(
        spectral_estimation_ui(id, methods=c("nonparam")),
              #box(
              #    title = "Principal components (2)", status = "primary", solidHeader = TRUE,
              #    collapsible = TRUE,
              #    plotOutput(ns("PCs"))
              #),
              box(
                  title = "Input time series", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(ns("tsPlot"))
              ),
        box(
                  title = "SPCA factors", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(ns("tsPCA"))
              )
          ),
          fluidRow(
              box(
                  title = "Input spectrum", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(ns("tsSpectrum"))
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




