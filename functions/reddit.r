#' @export
ui_subreddit <- function(id = "subreddit") {
  {
    box::use(shiny[tags, NS], shinycssloaders[withSpinner])
    box::use(shiny[actionButton, tableOutput, uiOutput, textInput, numericInput, fluidRow, div, column])

    box::use(. / utilities / datatable)
  }
  ns <- NS(id)
  fluidRow(
    column(3,
      class = "text-light p-2",
      textInput(ns("subreddit"), "Subreddit", "ukraine"),
      actionButton(ns("go"), "Go", class = "btn btn-primary btn-block text-dark")
    ),
    column(9, "Stuff")
  )
}

#' @export
server_subreddit <- function(id = "subreddit") {
  {
    box::use(shiny[moduleServer,showNotification, isolate, observe, req])
    box::use(. / utilities / datatable)
  }
  moduleServer(
    id,
    function(input, output, session) {
      {
        box::use(shiny[eventReactive, reactive, observeEvent, reactiveValues])
        box::use(. / reddit / reddit_pull[redpul_subreddit])
      }
      ns <- session$ns

      incoming <- reactive({
        input$go
        isolate(input$subreddit)
        tryCatch({
          out <- redpul_subreddit(name = input$subreddit)
        }, error = function(err) {
          showNotification(paste(c()))
          data.frame()
        })
      })

      incoming
    }
  )
}
