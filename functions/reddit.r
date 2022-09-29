#' @export
ui_subreddit <- function(id = "subreddit", container = function(...) shiny::column(12, ...)) {
  {
    box::use(shiny[tags,checkboxInput, NS, div], shinycssloaders[withSpinner])
    box::use(shiny[actionButton, tableOutput, uiOutput, textInput, numericInput, fluidRow, div, column])
    box::use(. / utilities / datatable)
  }
  ns <- NS(id)
  print(ns('subreddit'))
  container(
    class = "p-2",
    div(class='d-flex justify-content-around',
        textInput(ns("dbname"), "DB Name", "redditdb.sqlite"),
        textInput(ns("subreddit"), "Subreddit", "ukraine")),
    checkboxInput(ns('readdb'), 'Read from Database', TRUE),
    div(
      class='d-flex justify-content-between',
      actionButton(ns("dropDB"), "Drop DB", class='btn btn-primary'),
      actionButton(ns('go'), "Go", class = "btn btn-primary")
    )
  )
}

#' @export
server_subreddit <- function(id = "subreddit") {
  {
    box::use(shiny[moduleServer, showNotification, isolate, observe, req])
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
        tryCatch(
          {
            out <- isolate(redpul_subreddit(name = input$subreddit))
          },
          error = function(err) {
            data.frame()
          }
        )
      })


      filtered <- reactive({
        req(incoming())
        incoming_data <- incoming()

        out <-
          incoming_data |>
          dplyr::glimpse() |>
          dplyr::select(
            title, thumbnail, subreddit, author, score,
            num_comments, ups, downs, created_utc
          )
        out
      })

      observeEvent(input$dropDB, {
        file.remove(input$dbname)
        showNotification('Database deleted')
      })

      stored <- eventReactive(filtered(), {
        box::use(DBI, RSQLite)
        mydb <- DBI$dbConnect(RSQLite$SQLite(), input$dbname)
        if (!DBI$dbExistsTable(mydb, 'submissions')) {
          DBI$dbCreateTable(mydb, 'submissions', filtered())
        }
        DBI$dbAppendTable(mydb, 'submissions', filtered())
        DBI$dbDisconnect(mydb)
        showNotification('Data Stored')
      })


      dataset <- reactive({
        req(stored())
        input$readdb
        if (input$readdb) {
          showNotification('Reading from db')
          box::use(DBI, RSQLite, dplyr)
          mydb <- isolate(DBI$dbConnect(RSQLite$SQLite(), input$dbname))
          out <- dplyr$tbl(mydb, 'submissions') |> dplyr$collect()
          out
        } else {
          showNotification('Returning recently acquired data.')
          out <- filtered()
        }
        out
      })


      dataset
    }
  )
}
