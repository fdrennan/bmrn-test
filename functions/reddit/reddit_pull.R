#' @export
ui_subreddit <- function(id = "subreddit") {
  box::use(shiny, shinycssloaders[withSpinner], bs4Dash)
  box::use(shiny[actionButton, tableOutput, uiOutput, textInput, numericInput, row = fluidRow, div, col = column])
  ns <- shiny$NS(id)
  row(
    class = "vh-100",
    col(3,
      class = "bg-dark text-light p-2",
      numericInput(ns("limit"), "Limit", min = 1, max = 50, step = 1, value = 1),
      textInput(ns("subreddit"), "Subreddit", "ukraine"),
      actionButton(ns("go"), "Go", class = "btn btn-primary btn-block text-dark"),
      uiOutput(ns("ui"))
    ),
    col(9,
      class = "bg-light",
      row(
        col(12, withSpinner(tableOutput(ns("infoTable")), image = "./loaders/stats.gif"))
      )
    )
  )
}

#' @export
server_subreddit <- function(id = "subreddit") {
  box::use(shiny[row = fluidRow, moduleServer, div, col = column, tags, renderUI, req, renderTable, reactive, eventReactive, isolate])
  box::use(purrr[map, map_dfr], shinycssloaders, shinyWidgets[pickerInput], tidytext, jsonlite)
  box::use(. / reddit_pull[praw_subreddit])
  box::use(dplyr[select, filter, mutate])
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      subreddit_data <- reactive({
        input$go
        isolate(input$limit)
        isolate(input$subreddit)
        out <- praw_subreddit(limit = input$limit, name = input$subreddit)[[1]]
        map_dfr(names(out), function(name) {
          data.frame(
            name = name,
            type = class(out[name])
          )
        })
      })

      output$ui <- renderUI({
        col(
          12,
          pickerInput(
            inputId = ns("submissionNames"),
            label = "Select/deselect all",
            choices = {
              lapply(split(subreddit_data(), subreddit_data()$type), function(thing) {
                out <- list(thing$name)
                names(out) <- thing$type[[1]]
                out
              })
            },
            multiple = TRUE
          )
        )
      })

      output$infoTable <- renderTable({
        req(input$submissionNames)
        data <- subreddit_data()
        filter(data, name %in% input$submissionNames)
      })
    }
  )
}


#' @export
praw_subreddit <- function(name = getOption("subreddit", "all"),
                           limit = 10, type = c("new"),
                           client_id = Sys.getenv("REDDIT_CLIENT"),
                           client_secret = Sys.getenv("REDDIT_AUTH"),
                           user_agent = Sys.getenv("USER_AGENT")) {
  {
    box::use(dotenv[load_dot_env])
    box::use(reticulate[import, iterate])
    box::use(reticulate[iterate])
    box::use(purrr[map])
    box::use(dplyr[tibble])
  }
  praw <- import("praw")
  reddit <- praw$Reddit(client_id = client_id, client_secret = client_secret, user_agent = user_agent)
  subreddit <- reddit$subreddit(name)
  type <- match.arg(type)
  new_posts <- switch(type,
    new = subreddit$new(limit = limit)
  )
  new_posts <- iterate(new_posts)
}
