# Dependencies of the toolbox
install.packages(c(
    "BigVAR", "eegkit", "gdpc", 
    "ggbiplot", "ggplot2", 
    "glmnet", "HMClust", 
    "igraph", 
    "RColorBrewer", 
    "reshape2", 
    "shiny", "shinyWidgets", "shinydashboard", "shinyjs", 
    "vars"
))

remotes::install_github("r-lib/usethis")
remotes::install_github("r-lib/pkgdown")
remotes::install_github("hafen/rbokeh")
