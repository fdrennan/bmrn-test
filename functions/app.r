#' @export
sidebar_ui <- function() {
  box::use(shiny[tags, div, actionButton, icon])
  box::use(. / button)
  tags$nav(
    class = "d-none d-md-block sidebar bg-primary", style = "width: 4.5rem;",
    div(
      class = "sidebar-sticky",
      tags$ul(
        class = "nav flex-column",
        actionButton("home", icon("home", class = "py-2 fa-2x")),
        actionButton("aws", icon("aws", class = "py-2 fa-2x")),
        actionButton("settings", icon("cog", class = "py-2 fa-2x")),
        actionButton("full", icon("expand", class = "py-2  fa-2x"))
      )
    )
  )
}

#' @export
app_ui <- function(id = "app") {
  {
    box::use(
      shiny[addResourcePath, HTML, uiOutput, tags, div, fluidPage, column, NS, fluidRow, includeCSS, includeScript],
      shinyjs[useShinyjs, extendShinyjs],
      . / reddit,
      . / offcanvas,
      . / button,
      # . / button_toolbar[button_toolbar],
      esquisse,
      . / utilities / datatable
    )
    box::use(shiny[tags, actionButton, icon])
    box::use(. / app)
  }
  ns <- NS(id)

  # https://getbootstrap.com/docs/4.0/examples/dashboard/

  fluidPage(
    fluidRow(
      app$sidebar_ui(),
      tags$main(
        class = "mx-auto col-11 pt-3 px-4",
        uiOutput(ns("appBody"), container = function(...) {
          div(class = "col-12", ...)
        })
      )
    )
  )
}

#' @export
app_server <- function(id = "app") {
  box::use(shiny[moduleServer])
  box::use(shiny[observe, uiOutput, icon, actionButton, req, observeEvent, div, reactive, reactiveValues], shinyjs[js])
  box::use(shiny[fluidRow, column, renderUI])
  box::use(. / button)

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
        fluidRow(
          id = ns("maximize"),
          reddit$ui_subreddit(ns("subreddit"), container = function(...) {
            column(12, ...)
          })
        )
      })

      observe({
        input$full
        js$fullScreen(ns("maximize"))
      })

      subreddit_data <- reddit$server_subreddit()

      observe({
        req(subreddit_data())
        print(subreddit_data())
      })
    }
  )
}
