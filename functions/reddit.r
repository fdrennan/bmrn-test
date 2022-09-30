#' @export
ui_subreddit <- function(id = "subreddit", container = function(...) shiny::column(12, ...)) {
  {
    box::use(shiny[tags, checkboxInput, NS, div, icon], shinycssloaders[withSpinner])
    box::use(shiny[actionButton, tableOutput, uiOutput, textInput, numericInput, fluidRow, div, column])
    box::use(. / utilities / datatable)
  }
  ns <- NS(id)
  container(
    class = "p-2",
    div(
      class = "d-flex justify-content-around",
      textInput(ns("dbname"), "DB Name", "redditdb.sqlite"),
      textInput(ns("subreddit"), "Subreddit", "ukraine")
    ),
    checkboxInput(ns("readdb"), "Read from Database", TRUE),
    div(
      class = "d-flex justify-content-around",
      actionButton(ns("dropDB"), icon("dumpster-fire", class = "fa-2x"), class = "btn btn-warning p-2"),
      actionButton(ns("go"), tags$h1(icon("hand-spock", class = "fa-2x"), class = "btn btn-secondary p-2"))
    )
  )
}

#' @export
server_subreddit <- function(id = "subreddit") {
  {
    box::use(shiny[
      moduleServer, showNotification,
      isolate, observe, req
    ])
    box::use(dplyr)
    box::use(. / utilities / datatable)
    box::use(shinyjs)
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
        req(input$go)
        subreddit <- isolate(input$subreddit)
        tryCatch(
          {
            out <- isolate(redpul_subreddit(name = subreddit))
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
          dplyr$glimpse() |>
          dplyr$transmute(
            title, thumbnail,
            subreddit = as.factor(subreddit), author = as.factor(author), score,
            num_comments, ups, downs, created_utc = as.POSIXct(created_utc, origin = "1970-01-01")
          )
        out
      })

      observeEvent(input$dropDB, {
        file.remove(input$dbname)
        showNotification("Database deleted")
      })

      stored <- eventReactive(filtered(), {
        box::use(DBI, RSQLite)
        # req(input$dbname)
        # browser()
        mydb <- DBI$dbConnect(RSQLite$SQLite(), input$dbname)
        if (!DBI$dbExistsTable(mydb, "submissions")) {
          DBI$dbCreateTable(mydb, "submissions", filtered())
        }
        DBI$dbAppendTable(mydb, "submissions", filtered())
        DBI$dbDisconnect(mydb)
        showNotification("Data Stored")
      })


      dataset <- reactive({
        req(stored())
        input$readdb
        if (input$readdb) {
          showNotification("Reading from db")
          box::use(DBI, RSQLite, dplyr)
          mydb <- isolate(DBI$dbConnect(RSQLite$SQLite(), input$dbname))
          if (!DBI$dbExistsTable(mydb, "submissions")) {
            showNotification("Using data from prior pull, db does not exist.")
            return(filtered())
          }
          out <- dplyr$tbl(mydb, "submissions") |> dplyr$collect()
          out
        } else {
          showNotification("Returning recently acquired data.")
          out <- filtered()
        }
        out
      })


      dataset
    }
  )
}
