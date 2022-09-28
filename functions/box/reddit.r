#' @export
ui_subreddit <- function(id = "subreddit") {
  box::use(shiny[tags, NS], shinycssloaders[withSpinner])
  box::use(shiny[actionButton, tableOutput, uiOutput, textInput, numericInput, fluidRow, div, column])
  box::use(.. / utilities / datatable)
  ns <- NS(id)
  fluidRow(
    class = "vh-100",
    column(3,
      class = "bg-dark text-light p-2",
      textInput(ns("subreddit"), "Subreddit", "ukraine"),
      actionButton(ns("go"), "Go", class = "btn btn-primary btn-block text-dark")
    ),
    column(9,
      class = "bg-light",
      fluidRow(
        uiOutput(ns("typePicker"), container = function(...) column(12, ...)),
        uiOutput(ns("colPicker"), container = function(...) column(12, ...)),
        datatable$ui_dt(ns("submissionsTable"))
      )
    )
  )
}

#' @export
server_subreddit <- function(id = "subreddit") {
  {
    box::use(shiny[
      showNotification, actionButton, icon,
      moduleServer, div, selectizeInput, fluidRow, observeEvent, observe, column, tags, renderUI,
      req, renderTable, reactive, eventReactive, isolate
    ])
    box::use(purrr[map, map_dfr, map_dfc], jsonlite)
    box::use(.. / reddit / reddit_pull[praw_subreddit])
    box::use(.. / reddit / reddit_pull[redpul_subreddit])
    box::use(dplyr[select, filter, mutate])
    box::use(.. / utilities / datatable)
  }
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      incoming <- eventReactive(input$go, {
        out <- redpul_subreddit(name = input$subreddit)
      })

      observeEvent(input$go, {
        datatable$server_dt("submissionsTable", incoming())
      })
    }
  )
}
