# infoMethod to a tab page title
tabPageTitle <- function(infoMethod){
  title <- infoMethod$id
  value <- infoMethod$label
  subtitle <- NULL
  icon <- infoMethod$icon
  color <- infoMethod$color

  colorClass <- paste0("bg-", color)
  boxContent <- div(
    class = "info-box tab-page-title",
    span(
      class = "info-box-icon",
      class = colorClass,
      icon
    ),
    div(class = "info-box-content",
      span(class = "info-box-text", title),
      span(class = "info-box-number", value)
      #,
      #p(subtitle)
    )
  )
  boxContent
}
