#' ui_landing
#' @export
ui_landing <- function(id = "landing") {
  ns <- NS(id)
  base <- Sys.getenv("BASE_DOMAIN")
  div(
    class = "h-100 d-flex align-items-center justify-content-around",
    div(
      class = "p-3",
      style = "filter: brightness(0.5) sepia(1) hue-rotate(140deg) saturate(6);",
      imageOutput(ns("svgIcon"), height = "200px")
    ),
    fluidRow(
      class = "p-3",
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
      h3("By Quantitative Science, Data Science", class = "text-center my-4")
      # h3("By Quantitative Science, Data Science")
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
            src = normalizePath("test_logo.svg"),
            contentType = "image/svg+xml",
            height = 200,
            width = 200
          )
        },
        deleteFile = FALSE
      )
    }
  )
}
