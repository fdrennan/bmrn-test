#' @export
ui_administration <- function(id = "administration", container = shiny::fluidRow) {
  box::use(shiny[NS, uiOutput])
  ns <- NS(id)
  uiOutput(ns("ui"), container = container)
}

#' @export
server_administration <- function(id = "administration") {
  box::use(shiny[moduleServer])
  box::use(shiny[renderUI, renderPlot, plotOutput])
  box::use(shiny[fluidRow, column, h1, icon, tags])
  box::use(shiny[tabPanel, tabsetPanel])
  box::use(graphics[plot])
  box::use(. / aws[list_buckets])
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$ui <- renderUI({
        column(
          12,
          fluidRow(h1("AWS"), tags$p(tags$pre(list_buckets()))),
          fluidRow(h1("sqlite"))
        )
      })
    }
  )
}
