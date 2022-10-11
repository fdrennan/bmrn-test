#' @export
ui_hub <- function(id = "hub") {
  box::use(shiny)
  box::use(shiny[tags])
  ns <- shiny$NS(id)
  shiny$div(
    id = ns("maximize"),
    shiny$uiOutput(ns("app"))
  )
}

#' @export
server_hub <- function(id = "hub") {
  box::use(shiny)
  box::use(shiny[div, observeEvent])
  box::use(tibble)
  box::use(shinyjs[js])
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$app <- shiny$renderUI({
        shiny$h1("Hub")
      })
    }
  )
}
