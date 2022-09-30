
#' @export
app_ui <- function(id = "app") {
  {
    box::use(
      shiny[addResourcePath, uiOutput, tags, div, fluidPage, column, NS, fluidRow, includeCSS, includeScript],
      shinyjs[useShinyjs, extendShinyjs],
      . / reddit,
      . / offcanvas,
      . / button,
      . / button_toolbar[button_toolbar],
      esquisse,
      . / utilities / datatable
    )
    box::use(shiny[tags, actionButton, icon])
  }
  ns <- NS(id)


  fluidRow(
    column(
      12, uiOutput(ns("appBody"))
    ),
    # Offcanvas
    {
      div(
        offcanvas$offcanvas(
          id = ns("console"),
          location = "bottom",
          header = NULL,
          body = NULL,
          close_icon = "arrow-down"
        ),
        offcanvas$offcanvas(
          id = ns("settings"),
          location = "end",
          header = NULL,
          body = NULL,
          close_icon = "arrow-right"
        )
      )
    }
  )
}

#' @export
app_server <- function(id = "app") {
  box::use(shiny[moduleServer])
  box::use(shiny[observe, observeEvent, reactive, reactiveValues], shinyjs[js])
  box::use(shiny[fluidRow, column, renderUI])
  box::use(. / button_toolbar[button_toolbar])
  box::use(
    . / utilities / datatable,
    esquisse,
    . / reddit
  )
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$appBody <- renderUI({
        row_class <- c("border border-2 p-2 my-2")
        fluidRow(
          column(12,
            class = row_class,
            button_toolbar(
              ns,
              id = ns("button_toolbar")
            )
          ),
          reddit$ui_subreddit(ns("subreddit"), container = function(...) {
            column(6, ..., offset = 3)
          }),
          esquisse$esquisse_ui(ns("esquisse"), header = FALSE, container = function(...) {
            column(12, ..., style = "width: 100%; height:1700px;")
          }),
          datatable$ui_dt(ns("submissionsTable"))
        )
      })

      observe({
        input$full
        js$fullScreen("homepage")
      })

      subreddit_data <- reddit$server_subreddit()

      # esquisse$esquisse_server("esquisse",
      #   data_rv = reactiveValues(data = subreddit_data(), name = "subdata")
      # )
      observeEvent(subreddit_data(), {
        box::use(dplyr[select])
        out <- subreddit_data() |> select()
        datatable$server_dt("submissionsTable", subreddit_data())
      })
    }
  )
}
