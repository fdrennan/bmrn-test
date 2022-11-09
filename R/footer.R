#' @export
ui_footer <- function(id = "footer", container = function(...) {
                        column(12, ...)
                      }) {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("ui"), container = container)
}

#' @export
server_footer <- function(id = "footer") {
  box::use(shiny)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$ui <- shiny$renderUI({
        div(
          class = "d-flex justify-content-around",
          div(
            getOption("test_version")
          ),
          div(
            "© 2022 BioMarin"
          )
        )
      })
    }
  )
}
