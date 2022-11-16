#' ui_landing
#' @export
ui_landing <- function(id = "landing") {
  box::use(shiny)
  ns <- shiny$NS(id)
  base <- Sys.getenv("BASE_DOMAIN")
  shiny$fluidRow(
    shiny$column(5, shiny$imageOutput(ns("svgIcon"))),
    shiny$column(
      7,
      shiny$tags$br(),
      shiny$div(
        class = "d-flex justify-content-start",
        shiny$h2("Treatment", class = "underline-first-letter p-2"),
        shiny$h2("Evaluation", class = "underline-first-letter p-2"),
        shiny$h2("Statistical", class = "underline-first-letter p-2"),
        shiny$h2("Tools", class = "underline-first-letter p-2"),
        class = "font-weight-bold px-3"
      ),
      shiny$div(
        class = "d-flex justify-content-start",
        shiny$h2("for", class = " p-2"),
        shiny$h2("Preclinical", class = "p-2"),
        shiny$h2("Studies", class = "p-2"),
        class = "font-weight-bold px-3"
      ),
      shiny$tags$br(),
      shiny$tags$br(),
      shiny$tags$br(),
      shiny$h4("By Quantitative Science", class = "text-center"),
      shiny$div(
        style = "text-align: center",
        shiny$imageOutput(ns("dsIcon"), height = "200px")
      )
    )
  )
}

#' server_landing
#' @export
server_landing <- function(id = "landing") {
  box::use(shiny)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      output$svgIcon <- shiny$renderImage(
        {
          list(
            src = normalizePath("./www/test_logo.png"),
            contentType = "image/png",
            height = 500,
            width = 400,
            class = "align-center img-fluid"
          )
        },
        deleteFile = FALSE
      )

      output$dsIcon <- shiny$renderImage(
        {
          list(
            src = normalizePath("./www/dslogo.png"),
            contentType = "image/png",
            height = 200,
            width = 200
          )
        },
        deleteFile = FALSE
      )
    }
  )
}
