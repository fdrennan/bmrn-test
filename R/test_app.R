#' @export
ui_test_app <- function(id = "test_app") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("ui"))
}

#' @export
server_test_app <- function(id = "test_app") {
  box::use(shiny, bs4Dash)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$ui <- shiny$renderUI({
        bs4Dash$dashboardPage(
          fullscreen = FALSE, 
          dark = FALSE,
          header = ui_header(),
          body = ui_body(),
          sidebar = ui_sidebar(),
          footer = ui_footer()
        )
      })
      # server_body()
      # server_navbar()
      # server_footer()
    }
  )
}
