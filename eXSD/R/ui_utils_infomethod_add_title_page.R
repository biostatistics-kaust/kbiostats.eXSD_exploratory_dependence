# infoMethod to a tab page
addTitlePage <- function(infoMethod){
  tabname <- paste0("method_", infoMethod$id)
  infoTabElements = list(
    tabPageTitle(infoMethod)#,
  )
  if(!is.null(infoMethod$client)){
    infoTabElements[[length(infoTabElements) + 1]] <- infoMethod$client(infoMethod$id)
  }
  infoTabItem <- do.call(tabItem, c(tabName = tabname, class = "page-method", infoTabElements))
  infoTabItem
}


