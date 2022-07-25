#' ui_navbar
#' @export
ui_navbar <- function(id = "navbar", navbarId = "navbarNav") {
  BASE_DOMAIN <- Sys.getenv("BASE_DOMAIN")
  ns <- NS(id)
  id <- ns("navbarNav")
  id_hash <- paste0("#", id)
  div(
    class = "d-flex justify-content-between align-items-center p-3",
    actionButton(style='height: 100px',
      inputId = ns("goHome"),
      # class = "btn btn-default action-button",
      label = div(
        style = "width: 100px; height: 100px; filter: brightness(0.5) sepia(1) hue-rotate(140deg) saturate(6);",
        imageOutput(ns("svgIcon"), height='100px')
      )
    ),
    div(
      div(
        class = "d-flex justify-content-start",
        h1("Treatment", class = "underline-first-letter p-2"),
        h1("Evaluation", class = "underline-first-letter p-2"),
        h1("Statistical", class = "underline-first-letter p-2"),
        h1("Tools", class = "underline-first-letter p-2"),
        class = "font-weight-bold px-3"
      ),
      div(
        class = "d-flex justify-content-start",
        h1("for", class = " p-2"),
        h1("Research", class = "p-2"),
        h1("Experiments", class = "p-2"),
        class = "font-weight-bold px-3"
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
          width <- session$clientData$output_plot_width
          height <- session$clientData$output_plot_height
          mysvgwidth <- width / 96
          mysvgheight <- height / 96
          
          # A temp file to save the output.
          print(height)
          print(width)
          # Return a list containing the filename
          list(
            src = normalizePath("test_logo.svg"),
            contentType = "image/svg+xml",
            width = width,
            height = height
          )
        },
        deleteFile = FALSE
      )
      observeEvent(input$goHome, {
        change_page("home")
      })
      observeEvent(input$collapse, {
        print(reactiveValuesToList(input))
        id <- paste0("#", ns(navbarId))
        toggle(id)
      })
    }
  )
}