#' @export
ui_subreddit <- function(id = "subreddit", container = function(...) shiny::column(12, ...)) {
  {
    box::use(shiny[tags, checkboxInput, NS, div, icon], shinycssloaders[withSpinner])
    box::use(shiny[actionButton, tableOutput, uiOutput, textInput, numericInput, fluidRow, div, column])
    box::use(. / utilities / datatable)
    box::use(. / state / setDefault[setDefault])
    box::use(. / connections / storr)
  }
  username <- "fdrennan"
  con <- storr$connection_storr()
  ns <- NS(id)
  input <- tryCatch(con$get(ns("state")), error = function(err) {
    showNotification("No state exists", type = "warning")
    list()
  })
  readdb <- setDefault(input$readdb, TRUE)
  subreddit <- setDefault(input$subreddit, "all")
  poll <- setDefault(input$poll, FALSE)
  # col_class <- "col-xl-3 col-sm-6 col-xs-12 mx-auto  m-1 p-1 text-center"
  container(
    class = "p-1  vh-100",
    fluidRow(
      div(
        class = "col-7   p-1 text-center", tags$h3("Inputs"),
        div(
          class = "d-flex justify-content-around align-items-center",
          textInput(ns("subreddit"), NULL, subreddit),
          checkboxInput(ns("readdb"), icon("database", class = "fa-2x"), readdb),
          checkboxInput(ns("poll"), icon("play", class = "fa-2x"), poll)
        )
      ),
      div(
        class = "col-4 mx-auto  p-1 text-center", tags$h3("Actions"),
        div(
          class = "d-flex justify-content-around align-items-center",
          actionButton(ns("dropDB"), icon("dumpster-fire", class = "fa-2x"), class = "btn btn-warning p-1")
        )
      ),
      div(
        class = "col-12 mx-auto  p-1 text-center",
        div(
          class = "d-flex justify-content-around align-items-center",
          actionButton(ns("plots"), icon("chart-simple", class = "fa-2x"), class = "btn btn-link p-1"),
          actionButton(ns("data"), icon("table-cells", class = "fa-2x"), class = "btn btn-link p-1")
        )
      )
    ),
    fluidRow(
      uiOutput(ns("mainpanel"), container = function(...) {
        column(12, ...)
      })
    )
  )
}

#' @export
server_subreddit <- function(id = "subreddit") {
  {
    box::use(shiny[
      invalidateLater,
      moduleServer, showNotification,
      isolate, observe, req
    ])
    box::use(dplyr)
    box::use(. / utilities / datatable)
    box::use(shinyjs, esquisse)
  }
  moduleServer(
    id,
    function(input, output, session) {
      {
        box::use(shiny[
          eventReactive, renderUI, reactiveValuesToList, fluidRow,
          tags, reactive, observeEvent, reactiveValues
        ])
        box::use(. / reddit / reddit_pull[redpul_subreddit])
      }

      observe({
        box::use(. / connections / sqlite, storr)
        box::use(. / state / updateState)
        updateState$updateState(input, ns("state"))
      })

      ns <- session$ns
      incoming <- reactive({
        # req(input$go)
        req(is.logical(input$poll))
        input$go
        if (input$poll) {
          invalidateLater(5000)
          subreddit <- isolate(input$subreddit)
          tryCatch(
            {
              out <- isolate(redpul_subreddit(name = subreddit))
            },
            error = function(err) {
              data.frame()
            }
          )
        } else {
          req(FALSE)
        }
      })


      filtered <- reactive({
        req(incoming())
        box::use(lubridate)
        incoming_data <- incoming()
        out <-
          incoming_data |>
          dplyr$glimpse() |>
          dplyr$transmute(
            title, thumbnail,
            subreddit = as.factor(subreddit), author = as.factor(author), score,
            num_comments, ups, downs, created_utc = as.POSIXct(created_utc, origin = "1970-01-01"),
            date_char = as.character(created_utc),
            date_hour = lubridate$floor_date(Sys.time(), "hour"),
            date_minute = lubridate$floor_date(created_utc, "minute"),
            date_10_minute = lubridate$floor_date(created_utc, "10 minute"),
            date_30_minute = lubridate$floor_date(created_utc, "30 minute"),
            date_10_second = lubridate$floor_date(created_utc, "10 second"),
            date_30_second = lubridate$floor_date(created_utc, "30 second")
          )
        out
      })

      observeEvent(input$dropDB, {
        file.remove(getOption("ndexr_sqlite_path"))
        showNotification("poof")
      })

      stored <- eventReactive(filtered(), {
        box::use(DBI, RSQLite)
        box::use(. / connections / sqlite)
        con <- sqlite$connection_sqlite(getOption("ndexr_sqlite_path"))
        if (!DBI$dbExistsTable(con, "submissions")) {
          DBI$dbCreateTable(con, "submissions", filtered())
        }
        DBI$dbAppendTable(con, "submissions", filtered())
        DBI$dbDisconnect(con)
        showNotification("Data Stored")
      })


      dataset <- reactive({
        req(stored())
        input$readdb
        if (input$readdb) {
          showNotification("Reading from db")
          box::use(DBI, RSQLite, dplyr)
          con <- isolate(DBI$dbConnect(RSQLite$SQLite(), getOption("ndexr_sqlite_path")))
          if (!DBI$dbExistsTable(con, "submissions")) {
            showNotification("Using data from prior pull, db does not exist.")
            return(filtered())
          }
          out <- dplyr$tbl(con, "submissions") |> dplyr$collect()
          out
        } else {
          showNotification("Returning recently acquired data.")
          out <- filtered()
        }
        out
      })




      observe({
        req(dataset())
        if (input$poll) {
          output$mainpanel <- renderUI({
            nrow(dataset())
          })
        } else {
          output$mainpanel <- renderUI({
            esquisse$esquisse_ui(ns("esquisse"), header = FALSE, container = function(...) {
              fluidRow(tags$h3("Visualization"), ..., style = "height: 700px;")
            })
          })
          esquisse$esquisse_server("esquisse", data_rv = reactiveValues(data = dataset(), name = "subdata"))
        }
      })

      observeEvent(input$data, {
        browser()
        req(dataset())
        output$mainpanel <- renderUI({
          datatable$ui_dt(ns("submissionsTable"))
        })
        datatable$server_dt("submissionsTable", dataset())
      })

      dataset
    }
  )
}
