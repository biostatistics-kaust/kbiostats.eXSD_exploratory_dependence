# Convert infoMethods into sidebar items.
methodToSidebarItems <- function(infoMethods){
  sidebarItemMethods <- list()
  for (i in 1:length(infoMethods)) {
    infoMethod <- infoMethods[[i]]
    sidebarItemMethods[[i]] <- menuItem(
      infoMethod$label,
      tabName=paste0("method_", infoMethod$id),
      icon=infoMethod$icon
    )
  }
  methods <- sidebarMenu(
    .list=sidebarItemMethods
  )
  methods
}
