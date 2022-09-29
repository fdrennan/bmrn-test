



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
    fluidRow(
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
    fluidRow(
      column(
        4, reddit$ui_subreddit(ns('subreddit'))
      ),
      esquisse$esquisse_ui(ns("esquisse"), container = function(...) {
        column(8, ...)
      }),
      class = row_class
    ),
    fluidRow(
      class = row_class,
      datatable$ui_dt(ns("submissionsTable"))
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
app_server <- function(id='app') {
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

      observeEvent(subreddit_data(), {
        esquisse$esquisse_server("esquisse", data_rv = reactiveValues(data = subreddit_data(), name = "subdata"))
      })

      observeEvent(subreddit_data(), {
        box::use(dplyr[select])
        out <- subreddit_data() |> select()
        datatable$server_dt("submissionsTable", subreddit_data())
      })
    }
  )
}

#' @export
ui <- function() {
  box::use(. / app)
  {
    box::use(
      shiny[addResourcePath, tags, div, fluidPage, column, fluidRow, includeCSS, includeScript],
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
  addResourcePath("loaders", "./www/images/loaders")

  fluidPage(
    class = "bg-light",
    useShinyjs(),
    extendShinyjs(
      text = paste0(readLines("www/scripts/fullscreen.js"), collapse = "\n"), functions = "fullScreen"
    ),
    includeCSS("./www/styles.css"),
    includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
    column(
      id = "homepage",
      12,
      app$app_ui(id='app')
    )
  )
}

#' @export
server <- function(input, output, session) {
  box::use(. / app)
  app$app_server(id='app')
}

#' @export
start <- function() {
  box::use(shiny[runApp, shinyApp])
  box::use(. / app[ui, server])
  runApp(
    shinyApp(ui, server)
  )
}
