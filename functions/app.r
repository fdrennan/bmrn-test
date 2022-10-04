
#' @export
app_ui <- function(id = "app") {
  {
    box::use(
      shiny[
        addResourcePath, HTML, uiOutput, plotOutput, tags, div, fluidPage,
        column, NS, fluidRow
      ],
      shiny[includeCSS, includeScript, includeHTML],
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
    # http://jsfiddle.net/RichardHoultz/cxjje33y/
    class = "container-fluid",
    tags$header(
      class = "navbar navbar-dark sticky-top bg-dark flex-md-nowrap p-0 shadow",
      tags$a(class = "navbar-brand col-md-3 col-lg-2 me-0 px-3", href = "#", "ndexr"),
      tags$button(
        class = "navbar-toggler position-absolute d-md-none collapsed",
        type = "button", `data-bs-toggle` = "collapse",
        `data-bs-target` = "#sidebarMenu", `aria-controls` = "sidebarMenu",
        `aria-expanded` = "false", `aria-label` = "Toggle navigation",
        tags$span(class = "navbar-toggler-icon")
      ),
      includeHTML("www/html/navbar.html"),
      div(class = "navbar-nav", div(
        class = "nav-item text-nowrap",
        tags$a(class = "nav-link px-3", href = "#", "Sign Out")
      ))
    ),
    includeHTML("www/html/dashboard.html"),
    includeScript("www/scripts/dashboard.js"),
    div(class = "container-fluid", div(
      class = "row",
      tags$nav(
        id = "sidebarMenu", class = "col-md-3 col-lg-2 d-md-block bg-light sidebar collapse",
        includeHTML("www/html/sidebar.html")
      ),
      tags$main(
        class = "col-md-9 ms-sm-auto col-lg-10 px-md-4",
        includeHTML("www/html/dashboardMenu.html"),
        uiOutput(ns("subredditApp"), container = function(...) {
          div(class = "row", ...)
        })
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
