#' ui_navbar
#' @export
ui_navbar <- function(id = "navbar", navbarId = "navbarNav") {
  BASE_DOMAIN <- Sys.getenv("BASE_DOMAIN")
  ns <- NS(id)
  id <- ns("navbarNav")
  id_hash <- paste0("#", id)
  div(
    class = "d-flex justify-content-between align-items-center m-2 p-2",
    actionButton(
      style = "height: 100px",
      inputId = ns("goHome"),
      class = "p-1",
      label = div(
        # style = "filter: brightness(0.5) sepia(1) hue-rotate(140deg) saturate(6);",
        imageOutput(ns("svgIcon"), height = "90px")
      )
    ),
    div(
      div(
        class = "d-flex justify-content-start align-items-center",
        h2("Treatment", class = "underline-first-letter p-2"),
        h2("Evaluation", class = "underline-first-letter p-2"),
        h2("Statistical", class = "underline-first-letter p-2"),
        h2("Tools", class = "underline-first-letter p-2"),
        h2("for", class = " p-2"),
        h2("Preclinical", class = "p-2"),
        h2("Studies", class = "p-2"),
        class = "font-weight-bold p-1"
      )
    )
  )
}

#' server_navbar
#' @export
server_navbar <- function(id = "navbar", navbarId = "navbarNav") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$svgIcon <- renderImage(
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
      observeEvent(input$goHome, {
        change_page("home")
        reset()
      })
      observeEvent(input$collapse, {
        id <- paste0("#", ns(navbarId))
        toggle(id)
      })
    }
  )
}
