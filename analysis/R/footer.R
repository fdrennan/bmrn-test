#' @export
ui_footer <- function(id = "footer", container = function(...) {
                        shiny::column(12, ...)
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
        shiny$div(
          class = "d-flex justify-content-around",
          shiny$div(
            getOption("test_version")
          ),
          shiny$div(
            "© 2022 BioMarin"
          )
        )
      })
    }
  )
}
