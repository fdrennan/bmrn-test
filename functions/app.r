
#' @export
app_ui <- function(id = "app") {
  {
    box::use(
      shiny[addResourcePath, tags, div, fluidPage, column, NS, fluidRow, includeCSS, includeScript],
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

  row_class <- c("border border-5 border-dark p-2 my-2")
  fluidRow(
    column(12,
           class = row_class,
           button_toolbar(
             id = ns("button_toolbar"),
             button$button(
               label = icon("table"), class = "btn",
               id = ns("console"), data_bs_toggle = "offcanvas"
             ),
             button$button(
               label = icon("cog"), class = "btn",
               id = "settings", data_bs_toggle = "offcanvas"
             ),
             actionButton(ns("full"), icon("expand"))
           )
    ),
    reddit$ui_subreddit(ns("subreddit"), container = function(...) {
      column(6, ..., offset = 3)
    }),
    esquisse$esquisse_ui(ns("esquisse"), header = FALSE, container = function(...) {
      column(12, ..., style='width: 100%; height:1700px;')
    }),
    datatable$ui_dt(ns("submissionsTable")),
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

  moduleServer(
    id,
    function(input, output, session) {
      box::use(shiny[observe, observeEvent, reactive, reactiveValues], shinyjs[js])
      box::use(
        . / utilities / datatable,
        esquisse,
        . / reddit
      )
      observe({
        input$full
        js$fullScreen("homepage")
      })


      subreddit_data <- reddit$server_subreddit()

      esquisse$esquisse_server("esquisse",
                               data_rv = reactiveValues(data = subreddit_data(), name = "subdata")
      )
      observeEvent(subreddit_data(), {
        box::use(dplyr[select])
        out <- subreddit_data() |> select()
        datatable$server_dt("submissionsTable", subreddit_data())
      })
    }
  )
}


