
#' @export
ui_app <- function(id = "app") {
  {
    box::use(shiny)
    box::use(shiny[addResourcePath, NS])
    box::use(shiny[HTML, fluidPage, div, tags])
    box::use(shiny[fluidRow, column])
    box::use(shiny[plotOutput, uiOutput])
    box::use(shiny[actionButton, icon])
    box::use(shiny[includeCSS, includeScript, includeHTML])
    box::use(prompter)
    box::use(. / reddit)
    box::use(. / offcanvas)
    box::use(. / header)
    box::use(. / button)
    box::use(. / utilities / datatable)
    box::use(. / app)
    box::use(shiny.router[route_link])
  }
  ns <- NS(id)

  div(
    header$header_ui(),
    includeScript("www/scripts/dashboard.js"),
    prompter$use_prompt(),
    div(
      class = "row",
      tags$nav(
        id = "sidebarMenu", class = "col-md-3 col-lg-2 d-md-block bg-light sidebar collapse",
        div(
          class = "position-sticky h-100 pt-3 bg-dark",
          tags$h6("Apps", class = "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"),
          tags$ul(
            class = "nav flex-column",
            tags$li(
              class = "nav-item",
              div(
                id = ns("goToSubreddit"),
                tags$span(`data-feather` = "home", "Subreddit"),
                class = "nav-link action-button text-light"
              )
            )
          ),
          tags$h6("Consulting", class = "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"),
          tags$ul(
            class = "nav flex-column",
            tags$li(
              class = "nav-item",
              tags$a(class = "nav-link text-light", href = route_link("hub"), "Hub")
            ),
            tags$li(
              class = "nav-item",
              tags$a(class = "nav-link text-light", href = route_link("pigskin"), "Pigskin Analytics")
            )
          ),
          tags$h6("Build Status", class = "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"),
          tags$ul(
            class = "nav flex-column",
            tags$li(
              class = "nav-item",
              tags$a(
                class = "nav-link text-light", href = "https://gitlab.com/fdrennan/ndexr/-/commits/main",
                tags$img(alt = "pipeline status", src = "https://gitlab.com/fdrennan/ndexr/badges/main/pipeline.svg")
              )
            ),
            tags$li(
              class = "nav-item",
              tags$a(
                class = "nav-link text-light", href = "https://gitlab.com/fdrennan/ndexr/-/releases",
                tags$img(alt = "Latest Release", src = "https://gitlab.com/fdrennan/ndexr/-/badges/release.svg")
              )
            )
          )
        )
      ),
      tags$main(
        id = ns("maximize"),
        class = "col-md-9 ms-sm-auto col-lg-10 px-md-4",
        # includeHTML("www/html/dashboardMenu.html"),
        div(
          class = "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-4 pb-2 mb-3 border-bottom",
          shiny$h2("Dashboard"), shiny$actionButton(ns("fullscreen"), shiny::icon("expand"))
        ),
        uiOutput(ns("currentApp"), container = function(...) {
          div(style = "max-height: 100vh; overflow-y: auto;", class = "row", ...)
        })
      )
    )
  )
}

#' @export
server_app <- function(id = "app") {
  {
    box::use(shiny[moduleServer, observeEvent, div, reactive, reactiveValues])
    box::use(shiny[observe, uiOutput, renderPlot, icon, actionButton, req])
    box::use(shiny[fluidRow, HTML, tags, column, renderUI])
    box::use(jsonlite)
    box::use(shinyjs[js])
    box::use(. / button)
    box::use(. / utilities / datatable, esquisse, . / reddit)
    box::use(graphics)
    box::use(. / administration)
    box::use(. / nfl)
  }
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(input$fullscreen, {
        # browser()
        js$fullScreen(ns("maximize"))
      })

      observe({
        output$currentApp <- renderUI({
          tags$h3("References")
        })
      })

      observeEvent(input$goToSubreddit, {
        output$currentApp <- renderUI({
          reddit$ui_subreddit(ns("subreddit"), container = function(...) {
            column(12, ...)
          })
        })
        reddit$server_subreddit()
      })
    }
  )
}
