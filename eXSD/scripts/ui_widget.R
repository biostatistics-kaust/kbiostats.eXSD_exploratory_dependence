
#' @export
ui_widget <- function(datasources, labels) {
  stopifnot(length(datasources) == length(labels))
  stopifnot(all(names(datasources) == names(labels)))
  codes <- names(datasources)
  choiceNames <- unlist(labels)
  names(choiceNames) <- NULL
  infoMethods <- collect_methods()
  datasources_widget <- radioButtons(
     "chkDataSource",
     NULL,
     choiceNames=choiceNames, 
     choiceValues=codes
  )
  startTabs <- list(
    tabItem(tabName = "main",
      methodToValueBoxes(infoMethods)
    )
  )
  body <- dashboardBody(
    withMathJax(),
    tags$head(
      shiny::includeCSS(system.file("www/custom.css", package="DEXplorer")),
      shiny::includeScript(system.file("www/custom.js", package="DEXplorer"))
    ),
    methodToTabItems(infoMethods, startTabs),
    tags$div(class="app-footer",
      span("XAD :: Exploratory Analysis of Dependency in Time Series."), 
      a(href="http://doi.org/0000.0000/0000.0000", "doi:0000.0000/0000.0000")
    )
  )
  ui <- dashboardPage(
    skin="black",
    dashboardHeader(
      title = "Dependency measures in time series"
    ),
    dashboardSidebar(
      sidebarMenu(menuItem("Main panel", tabName = "main", icon = icon("dot-circle-o"))),
      sidebarLabelItem(class = "ui-label", label = "Datasets"),
      datasources_widget, 
      sidebarLabelItem(class = "ui-label", label = "Methods"),
      tags$div(
        class="height-restricted-sidebar",
        methodToSidebarItems(infoMethods)
      )
    ),
    body
  )
}

sidebarLabelItem <- function(class, label) div(class=class, p(label))


