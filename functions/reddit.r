#' @export
ui_subreddit <- function(id = "subreddit") {
  {
    box::use(shiny[tags, NS], shinycssloaders[withSpinner])
    box::use(shiny[actionButton, tableOutput, uiOutput, textInput, numericInput, fluidRow, div, column])
    box::use(esquisse)
    box::use(. / utilities / datatable)
  }
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
        esquisse$esquisse_ui(id=ns('esquisse')),
        datatable$ui_dt(ns("submissionsTable"))
      )
    )
  )
}

#' @export
server_subreddit <- function(id = "subreddit") {
  {
    box::use(shiny[moduleServer])
    box::use(. / utilities / datatable)
  }
  moduleServer(
    id,
    function(input, output, session) {
      {
        box::use(esquisse)
        box::use(shiny[eventReactive, observeEvent, reactiveValues])
        box::use(. / reddit / reddit_pull[redpul_subreddit])
      }
      ns <- session$ns

      incoming <- eventReactive(input$go, {
        out <- redpul_subreddit(name = input$subreddit)
      })

      observeEvent(input$go, {
        datatable$server_dt("submissionsTable", incoming())
        esquisse$esquisse_server('esquisse', data_rv = reactiveValues(data=incoming(), name = 'subdata'))
      })
    }
  )
}
