# Convert infoMethods into tab items.
methodToTabItems <- function(infoMethods, baselist=NULL){
  tabItemMethods <- list()
  n <- 0
  if(!is.null(baselist)){
    n <- length(baselist)
    for (i in 1:n) {
      tabItemMethods[[i]] <- baselist[[i]]
    }
  }
  for (i in 1:length(infoMethods)) {
    infoMethod <- infoMethods[[i]]
    #print(class(infoMethod$page))
    tabItemMethods[[i + n]] <- addTitlePage(infoMethod)
  }
  #print(class(tabItemMethods))
  #print(length(tabItemMethods))
  tabMethods <- do.call(tabItems, tabItemMethods)
  #print(class(tabMethods))
  #print(tabMethods)
  #return(p("aab"))
  #tabMethods <- tabItems(tabItemMethods)
  tabMethods
}

