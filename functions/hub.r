#' @export
ui_hub <- function(id = "hub") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("ui"))
}

#' @export
server_hub <- function(id = "hub") {
  box::use(shiny)
  box::use(shiny[div])

  box::use(tibble)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$ui <- shiny$renderUI({
        div(
          "Hello"
        )
      })
    }
  )
}
