#' @export
matrix_as_network <- function(X, colfmt="ch%02d", force_name=FALSE){
  r <- nrow(X)
  c <- ncol(X)
  X <- add_colnames_to_matrix(abs(X), fmt=colfmt, force_name=force_name)
  igraph::graph_from_adjacency_matrix(X)
}

 mat2graph222 <- function(m, preffix="ch.", m_colnames=NULL){
    g <- as.matrix(m)
    g <- g - diag(diag(g))
    colnames(g) <- m_colnames
    rownames(g) <- m_colnames
    G <- as.directed(graph.adjacency(g, weighted = T))
    com <- cluster_spinglass(G, spins=5)
    w <- E(G)$weight
    w <- magnitude_factor * (w - min(w)) / (max(w) - min(w))
    #w <- w[w > quantile(w, 0.75)]
    E(G)$weight <- w
    E(G)$width <- w
    E(G)$curve <- w * 0 + 2
    V(G)$color <- "#5cbdf1"
    G
}

#' @export
matrix_as_network_percentile <- function(X, percentile=0.5, magnitude_factor=5, colfmt="ch%02d", force_name=FALSE, is_directed=TRUE){
  r <- nrow(X)
  c <- ncol(X)
  # TODO: Add checking
  X <- add_colnames_to_matrix(abs(X), fmt=colfmt, force_name=force_name)
  rownames(X) <- colnames(X)
  diag(X) <- 0
  if(!is_directed){
    X[lower.tri(X)] <- 0
  }
  cutoff <- quantile(X, percentile)
  X[X < cutoff] <- 0
  X <- X / max(X + 1e-10)
  G <- igraph::graph_from_adjacency_matrix(X, weighted=TRUE)
  E(G)$weight  <- E(G)$weight * magnitude_factor
  E(G)$width  <- E(G)$weight * magnitude_factor
  V(G)$color <- "#5cbdf1"
  #print(X)
  #print(E(G)$weight)
  #print(G)
  #print("-----")
  G
}

#' @export
basicNetworkWidget <- function(preffix){
  labelCutoffSlider <- paste0("txt", preffix, "CutoffSlide")
  networkSlider <- paste0("txt", preffix, "Network")
  client <- function(ns){
    tags$div(class='network-widget',
      box(
          title = "", status = "primary", solidHeader = FALSE, width=12,
          collapsible = FALSE,
          plotOutput(ns(networkSlider)),
          sliderInput(ns(labelCutoffSlider), "Filter edges by quantile:", 0, 1, 0.5, step=0.01)
          #helpText("Edges with magnitudes in the upper quartile")
      )
    )
  }
  server <- function(input, output, getMatrixFunctor, magnitude_factor=2, is_directed=TRUE){
    output[[networkSlider]] <- renderPlot({
        #print(list(percentile=input[[labelCutoffSlider]]))
        G <- matrix_as_network_percentile(getMatrixFunctor(), percentile=input[[labelCutoffSlider]], magnitude_factor=magnitude_factor, is_directed=is_directed)
        plot(G, edge.arrow.size=(if(is_directed) 1 else 0), vertex.size=40, edge.curved=TRUE)
    }, res=250, width = 3.5*700, height = 3.5*500)
  }
  list(
    client=client,
    server=server
  )
}


