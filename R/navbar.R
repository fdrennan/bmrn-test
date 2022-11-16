#' ui_navbar
#' @export
ui_navbar <- function(id = "navbar", navbarId = "navbarNav") {
  box::use(shiny)
  BASE_DOMAIN <- Sys.getenv("BASE_DOMAIN")
  ns <- shiny$NS(id)
  id <- ns("navbarNav")
  id_hash <- paste0("#", id)
  shiny$div(
    class = "d-flex justify-content-between align-items-center m-2 p-2",
    shiny$actionButton(
      style = "height: 100px",
      inputId = ns("goHome"),
      class = "p-1",
      label = shiny$div(
        # style = "filter: brightness(0.5) sepia(1) hue-rotate(140deg) saturate(6);",
        shiny$imageOutput(ns("svgIcon"), height = "90px")
      )
    ),
    shiny$div(
      shiny$div(
        class = "d-flex justify-content-start align-items-center",
        shiny$h2("Treatment", class = "underline-first-letter p-2"),
        shiny$h2("Evaluation", class = "underline-first-letter p-2"),
        shiny$h2("Statistical", class = "underline-first-letter p-2"),
        shiny$h2("Tools", class = "underline-first-letter p-2"),
        shiny$h2("for", class = " p-2"),
        shiny$h2("Preclinical", class = "p-2"),
        shiny$h2("Studies", class = "p-2"),
        class = "font-weight-bold p-1"
      )
    )
  )
}

#' server_navbar
#' @export
server_navbar <- function(id = "navbar", navbarId = "navbarNav") {
  box::use(shiny)
  box::use(shiny.router)
  box::use(shinyjs)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$svgIcon <- shiny$renderImage(
        {
          list(
            src = normalizePath("./www/test_logo.png"),
            contentType = "image/png",
            width = 80,
            height = 100
          )
        },
        deleteFile = FALSE
      )
      shiny$observeEvent(input$goHome, {
        shiny.router$change_page("home")
        shinyjs$reset()
      })
      shiny$observeEvent(input$collapse, {
        id <- paste0("#", ns(navbarId))
        shinyjs$toggle(id)
      })
    }
  )
}
