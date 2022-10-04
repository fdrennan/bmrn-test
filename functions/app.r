
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
      . / header,
      . / button,
      . / sidebar,
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
    header$header_ui(),
    includeScript("www/scripts/dashboard.js"),
    div(class = "container-fluid", div(
      class = "row",
      tags$nav(
        id = "sidebarMenu", class = "col-md-3 col-lg-2 d-md-block bg-light sidebar collapse",
        div(
          class = "position-sticky pt-3",
          tags$h6("Reddit Search", class = "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"),
          tags$ul(
            class = "nav flex-column",
            tags$li(
              class = "nav-item",
              div(
                id = ns("goToSubreddit"),
                tags$span(`data-feather` = "home", "Subreddit"),
                class = "nav-link active action-button"
              )
            ),
            tags$li(
              class = "nav-item",
              div(
                id = ns("goToAuthor"),
                tags$span(`data-feather` = "home", "Author"),
                class = "nav-link action-button"
              )
            )
          )
        ),
        div(
          class = "position-sticky pt-3",
          tags$h6("Administration", class = "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"),
          tags$ul(
            class = "nav flex-column",
            tags$li(
              class = "nav-item",
              tags$a(
                class = "nav-link", href = "#",
                tags$span(`data-feather` = "home", "Connections")
              )
            )
          )
        )
      ),
      tags$main(
        class = "col-md-9 ms-sm-auto col-lg-10 px-md-4",
        includeHTML("www/html/dashboardMenu.html"),
        uiOutput(ns("currentApp"), container = function(...) {
          div(class = "row", ...)
        })
      )
    ))
  )
}

#' @export
app_server <- function(id = "app") {
  {
    box::use(shiny[moduleServer, observeEvent, div, reactive, reactiveValues])
    box::use(shiny[observe, uiOutput, renderPlot, icon, actionButton, req])
    box::use(shiny[fluidRow, tags, column, renderUI])
    box::use(jsonlite)
    box::use(shinyjs[js])
    box::use(. / button)
    box::use(. / sidebar)
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

      observe({
        input$goToSubreddit
        output$currentApp <- renderUI({
          reddit$ui_subreddit(ns("subreddit"), container = function(...) {
            column(12, ...)
          })
        })

        reddit$server_subreddit()
      })

      observeEvent(input$goToAuthor, {
        output$currentApp <- renderUI({
          tags$pre(jsonlite$toJSON(as.list(Sys.getenv()), pretty = TRUE))
        })
      })


      observe({
        input$full
        js$fullScreen(ns("maximize"))
      })
    }
  )
}
