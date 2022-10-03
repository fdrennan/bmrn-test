
#' @export
app_ui <- function(id = "app") {
  {
    box::use(
      shiny[addResourcePath, HTML, uiOutput, tags, div, fluidPage, column, NS, fluidRow, includeCSS, includeScript],
      shinyjs[useShinyjs, extendShinyjs],
      . / reddit,
      . / offcanvas,
      . / button,
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
      id = ns("maximize"),
      tags$nav(
        class = "d-block sidebar", style = "width: 4.5rem;",
        div(
          class = "sidebar-sticky p-1",
          tags$ul(
            class = "nav flex-column",
            actionButton(ns("home"), icon("home", class = "py-2 fa-2x")),
            actionButton(ns("aws"), icon("aws", class = "py-2 fa-2x")),
            actionButton(ns("settings"), icon("cog", class = "py-2 fa-2x")),
            actionButton(ns("full"), icon("expand", class = "py-2  fa-2x"))
          )
        )
      ),
      tags$main(
        class = "mx-auto col-10 p-4",
        uiOutput(ns("appBody"), container = function(...) {
          div(class = "row", ...)
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

  box::use(. / utilities / datatable, esquisse, . / reddit)
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$appBody <- renderUI({
        reddit$ui_subreddit(ns("subreddit"), container = function(...) {
          column(12, ...)
        })
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
