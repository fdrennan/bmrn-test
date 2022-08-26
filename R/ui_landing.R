#' ui_landing
#' @export
ui_landing <- function(id = "landing") {
  ns <- NS(id)
  base <- Sys.getenv("BASE_DOMAIN")
  fluidRow(
    column(5,
      # style = "my-5",
      class = "d-flex justify-content-center align-items-center flex-col",
      imageOutput(ns("svgIcon"), width = "60%")
    ),
    column(
      7,
      tags$br(),
      div(
        class = "d-flex justify-content-start",
        h2("Treatment", class = "underline-first-letter p-2"),
        h2("Evaluation", class = "underline-first-letter p-2"),
        h2("Statistical", class = "underline-first-letter p-2"),
        h2("Tools", class = "underline-first-letter p-2"),
        class = "font-weight-bold px-3"
      ),
      div(
        class = "d-flex justify-content-start",
        h2("for", class = " p-2"),
        h2("Preclinical", class = "p-2"),
        h2("Studies", class = "p-2"),
        class = "font-weight-bold px-3"
      ),
      tags$br(),
      tags$br(),
      tags$br(),
      h4("By Quantitative Science", class = "text-center"),
      div(
        style = "text-align: center",
        imageOutput(ns("dsIcon"), height = "200px")
      )
    )
  )
}

#' server_landing
#' @export
server_landing <- function(id = "landing") {
  moduleServer(
    id,
    function(input, output, session) {
      output$svgIcon <- renderImage(
        {
          list(
            src = normalizePath("test_logo.png"),
            contentType = "image/png",
            height = 500,
            width = 400
          )
        },
        deleteFile = FALSE
      )

      output$dsIcon <- renderImage(
        {
          list(
            src = normalizePath("dslogo.png"),
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
