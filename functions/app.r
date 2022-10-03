
#' @export
app_ui <- function(id = "app") {
  {
    box::use(
      shiny[addResourcePath, HTML, uiOutput, plotOutput, tags, div, fluidPage, column, NS, fluidRow, includeCSS, includeScript],
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


  div(
    class = "container-fluid",
    shiny::includeHTML("www/html/navbar.html"),
    shiny::includeHTML("www/html/dashboard.html"),
    shiny::includeScript("www/scripts/dashboard.js"),
    div(class = "container-fluid", div(
      class = "row",
      shiny::includeHTML("www/html/sidebar.html"),
      tags$main(
        class = "col-md-9 ms-sm-auto col-lg-10 px-md-4",
        shiny::includeHTML("www/html/dashboardMenu.html"),
        shinycssloaders::withSpinner(
          uiOutput(ns("subredditApp"), container = function(...) {
            div(class = "row", ...)
          })
        )
        # tags$canvas(class="my-4 w-100 shiny-plot-output", id='myChart', width="900", height="380")
      )
    ))
    # fluidRow(
    # id = ns("maximize"),

    # tags$div(
    #   class = "col-1 bg-dark",
    #   fluidRow(
    #     actionButton(ns("home"), icon("home", class = "text-secondary")),
    #     actionButton(ns("aws"), icon("aws", class = "text-secondary")),
    #     actionButton(ns("settings"), icon("cog", class = "text-secondary")),
    #     actionButton(ns("full"), icon("expand", class = "text-secondary"))
    #   )
    # ),
    # tags$main(
    #   class = "mx-auto col-10 p-4",
    #   uiOutput(ns("appBody"), container = function(...) {
    #     div(class = "row", ...)
    #   })
    # )
    # )
  )
}

#' @export
app_server <- function(id = "app") {
  {
    box::use(shiny[moduleServer, observeEvent, div, reactive, reactiveValues])
    box::use(shiny[observe, uiOutput, renderPlot, icon, actionButton, req])
    box::use(shiny[fluidRow, column, renderUI])
    box::use(shinyjs[js])
    box::use(. / button)
    box::use(. / utilities / datatable, esquisse, . / reddit)
    box::use(graphics)
  }
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$myChart <- renderPlot({
        graphics$plot(1:10, 1:10)
      })

      output$subredditApp <- renderUI({
        reddit$ui_subreddit(ns("subreddit"), container = function(...) {
          column(12, ...)
        })
      })

      reddit$server_subreddit()


      observe({
        input$full
        js$fullScreen(ns("maximize"))
      })
    }
  )
}
