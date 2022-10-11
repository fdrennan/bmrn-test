#' @export
ui_hub <- function(id = "hub") {
  box::use(shiny)
  box::use(shiny[tags])
  ns <- shiny$NS(id)
  shiny$div(
    id = ns("maximize"),
    # tags$head(shinyjs::useShinyjs()),
    shiny$textInput(ns("name_set"), "What is your name?"),
    shiny$actionButton(ns("save"), "Save cookie"),
    shiny$actionButton(ns("remove"), "remove cookie"),
    shiny$uiOutput(ns("name_get")),
    shiny$actionButton(ns("full"), "Big Hub")
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

      observeEvent(input$full, {
        # browser()
        js$fullScreen(ns("maximize"))
      })

      shiny$observeEvent(input$save, {
        browser()
      })
    }
  )
}
