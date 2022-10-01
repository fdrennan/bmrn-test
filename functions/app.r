#' @export
sidebar_ui <- function() {
  box::use(shiny[tags, div])
  tags$nav(
    class = "col-md-2 d-none d-md-block bg-light sidebar",
    div(
      class = "sidebar-sticky",
      tags$ul(
        class = "nav flex-column",
        tags$li(
          class = "nav-item",
          tags$a(
            class = "nav-link active",
            "Home"
          )
        )
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
        class = "col-md-9 ml-sm-auto col-lg-10 pt-3 px-4",
        uiOutput(ns("appBody"), container = function(...) {
          div(class = "col-12", ...)
        })
      )
    ),
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
  )
}

#' @export
app_server <- function(id = "app") {
  box::use(shiny[moduleServer])
  box::use(shiny[observe, uiOutput, icon, actionButton, req, observeEvent,div, reactive, reactiveValues], shinyjs[js])
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
          div(
            class = "btn-toolbar d-flex justify-content-start bg-light",
            role = "toolbar",
            `aria-label` = "Top application toolbar",
            div(
              class = "btn-group", role = "group", `aria-label` = "First group",
              button$button(
                label = icon("aws", class = "fa-2x"), class = "btn",
                id = ns("console"), data_bs_toggle = "offcanvas"
              ),
              button$button(
                label = icon("cog", class = "fa-2x"), class = "btn",
                id = ns("settings"), data_bs_toggle = "offcanvas"
              ),
              actionButton(
                ns("full"),
                icon("expand", class = "fa-2x")
              )
            )
          ),
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
