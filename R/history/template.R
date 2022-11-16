
#' ui_body
#' @export
ui_template <- function(id = "main_panel") {
  ns <- NS(id)
  div(
    class = "main-panel",
    uiOutput(ns("body"))
  )
}


#' server_body
#' @export
server_template <- function(id = "main_panel") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$body <- renderUI({
        # filler <- filler_text(3)
        fluidRow(
          wellPanel(
            selectizeInput("panel", "Well Panel", choices = c("html", "code"), "html")
          ),
          inputPanel(
            selectizeInput("panel", "Input Panel", choices = c("html", "code"), "html")
          ),
          imap(
            list(
              h1 = h1,
              h2 = h2,
              h3 = h3,
              h4 = h4,
              h5 = h5,
              h6 = h6,
              p =shiny$tags$p,
              pre =shiny$tags$pre,
              em =shiny$tags$em,
              span =shiny$tags$span,
              strong =shiny$tags$strong,
              code =shiny$tags$code
            ), function(x, i) {
              fluidRow(
                class = "fallin",
                column(
                  3,
                  div(h4(i), class = "flex-center centerit")
                ),
                column(
                  9,
                  x("Example <- -> == ===")
                )
              )
            }
          )
        )
      })
    }
  )
}
