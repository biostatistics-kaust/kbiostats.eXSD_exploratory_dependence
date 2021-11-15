# Infomethods to value boxes
methodToValueBoxes <- function(infoMethods){
  mainButtonItems <- list()
  for (i in 1:length(infoMethods)) {
    infoMethod <- infoMethods[[i]]
    mainButtonItems[[i]] <- valueBox(
      value=infoMethod$id,
      subtitle=infoMethod$label,
      href=paste0("#shiny-tab-method_", infoMethod$id),
      color=infoMethod$color,
      icon=infoMethod$icon
    )
  }
  mainButtons <- do.call(tags$div, c(class = "row main-methods", mainButtonItems))
  mainButtons
}
