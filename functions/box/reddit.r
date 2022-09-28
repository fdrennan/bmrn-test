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
           numericInput(ns("limit"), "Limit", min = 1, max = 50, step = 1, value = 10),
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
    box::use(shiny[showNotification,actionButton,icon,
                   moduleServer, div, selectizeInput, fluidRow, observeEvent, observe, column, tags, renderUI,
                   req, renderTable, reactive, eventReactive, isolate
    ])
    box::use(purrr[map, map_dfr, map_dfc], jsonlite)
    box::use(.. / reddit /reddit_pull[praw_subreddit])
    box::use(.. / reddit /reddit_pull[redpul_subreddit])
    box::use(dplyr[select, filter, mutate])
    box::use(.. / utilities / datatable)
  }
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      incoming <- eventReactive(input$go, {
        out <- praw_subreddit(limit = input$limit, name = input$subreddit)
      })
      subreddit_data <- reactive({
        req(incoming())
        out <- incoming()[[1]]
        map_dfr(names(out), function(name) {
          data.frame(
            name = name,
            type = class(out[name])
          )
        })
      })

      observeEvent(subreddit_data(), {
        req(subreddit_data())
        subreddit_data()

        output$typePicker <- renderUI({
          selectizeInput(
            ns("types"),
            "Types",
            choices = subreddit_data()$type,
            selected = c("character", "integer", "numeric"),
            multiple = TRUE
          )
        })
      })


      observeEvent(input$types, {
        subreddit_data <- subreddit_data()[subreddit_data()$type %in% input$types, ]
        output$colPicker <- renderUI({
          div(
            selectizeInput(
              ns("name"),
              "Name",
              choices = subreddit_data$name,
              selected = c('permalink', 'title', 'selftext'),
              multiple = TRUE
            ),
            actionButton(ns('updateTable'), div(class='text-right', icon('check')))
          )
        })
      })

      observeEvent(input$updateTable, {
        datatable$server_dt("submissionsTable", {
          map_dfr(
            incoming(),
            function(submission) {
              keep_names <- names(submission)[names(submission) %in% input$name]
              tryCatch({
                out <- map_dfc(
                  keep_names, function(x) {
                    out <- data.frame(submission[[x]])
                    names(out) <- x
                    out
                  }
                )
                out
              }, error = function(err) {showNotification('Failure');data.frame()})
            }
          )
        })
      })
    }
  )
}

