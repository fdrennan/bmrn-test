#' @export
ui_clock <- function(id = "clock") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("ui"))
}

#' @export
server_clock <- function(id = "clock") {
  box::use(shiny[moduleServer])
  moduleServer(
    id,
    function(input, output, session) {
      box::use(shiny[invalidateLater, renderUI, div])
      ns <- session$ns
      output$ui <- renderUI({
        invalidateLater(60000)
        div(as.character.POSIXt(Sys.time()))
      })
    }
  )
}
