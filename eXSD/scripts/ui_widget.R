
#' @export
ui_widget <- function(datasources, labels, dataparameters) {
  stopifnot(length(datasources) == length(labels))
  stopifnot(all(names(datasources) == names(labels)))
  codes <- names(datasources)
  choiceNames <- unlist(labels)
  names(choiceNames) <- NULL
  infoMethods <- collect_methods(dataparameters)
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
      shiny::includeCSS(system.file("www/custom.css", package="eXSD")),
      shiny::includeScript(system.file("www/custom.js", package="eXSD")),
      tags$head(tags$link(rel="shortcut icon", href=system.file("www/favicon.ico", package="eXSD")))
    ),
    methodToTabItems(infoMethods, startTabs),
    tags$div(class="app-footer",
      span("XAD :: Exploratory Analysis of Dependency in Time Series."), 
      a(href="https://arxiv.org/abs/2103.17240", "arxiv:2103.17240")
    )
  )
  ui <- dashboardPage(
    skin="black",
    dashboardHeader(
      title = "Dependency measures in time series"
    ),
    dashboardSidebar(
      sidebarMenu(menuItem("Main panel", tabName = "main", icon = shiny::icon("dot-circle-o"))),
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


