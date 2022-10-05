
#' @export
app_ui <- function(id = "app") {
  {
    box::use(shiny[addResourcePath, NS])
    box::use(shiny[HTML, fluidPage, div, tags])
    box::use(shiny[fluidRow, column])
    box::use(shiny[plotOutput, uiOutput])
    box::use(shiny[actionButton, icon])
    box::use(shiny[includeCSS, includeScript, includeHTML])
    box::use(shinyjs[useShinyjs, extendShinyjs])
    box::use(esquisse)
    box::use(. / reddit)
    box::use(. / offcanvas)
    box::use(. / header)
    box::use(. / button)
    box::use(. / sidebar)
    box::use(. / utilities / datatable)
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
                class = "nav-link action-button"
              )
            )
          )
        ),
        div(
          class = "position-sticky pt-3",
          tags$h6("Infrastructure", class = "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"),
          tags$ul(
            class = "nav flex-column",
            tags$li(
              class = "nav-item",
              div(
                id = ns("goToAdministration"),
                tags$span(`data-feather` = "home", "Administration"),
                class = "nav-link action-button"
              )
            )
          ),
          tags$ul(
            class = "nav flex-column",
            tags$li(
              class = "nav-item",
              div(
                id = ns("goToDevelopment"),
                tags$span(`data-feather` = "home", "Development"),
                class = "nav-link action-button"
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
    box::use(shiny[fluidRow, HTML, tags, column, renderUI])
    box::use(jsonlite)
    box::use(shinyjs[js])
    box::use(. / button)
    box::use(. / sidebar)
    box::use(. / utilities / datatable, esquisse, . / reddit)
    box::use(graphics)
    box::use(. / administration)
  }
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(input$full, {
        js$fullScreen(ns("maximize"))
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

      observeEvent(input$goToAdministration, {
        output$currentApp <- renderUI({
          administration$ui_administration(ns("administration"), container = function(...) {
            column(12, ...)
          })
        })
        administration$server_administration()
      })

      observeEvent(input$goToDevelopment, {
        output$currentApp <- renderUI({
          fluidRow(
            HTML('<a href="https://gitlab.com/fdrennan/ndexr/-/releases"><img alt="Latest Release" src="https://gitlab.com/fdrennan/ndexr/-/badges/release.svg" /></a>'),
            HTML('<a href="https://gitlab.com/fdrennan/ndexr/-/commits/main"><img alt="pipeline status" src="https://gitlab.com/fdrennan/ndexr/badges/main/pipeline.svg" /></a>')
          )
        })
      })
    }
  )
}
