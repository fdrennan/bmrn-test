#' @export
start_app <- function() {
  plan(multiprocess)
  options("test_version" = "Version 1.21")

  if (isTRUE(getOption("production"))) {
    options(shiny.maxRequestSize = 300 * 1024^2)
    options(require_validation = TRUE)
    options(send = TRUE)
    options("devmode" = FALSE)
  } else {
    options(require_validation = FALSE)
    options(send = FALSE)
    options("devmode" = TRUE)
  }


  ui <- function() {
    ui_test_app()
  }

  server <- function(input, output, session) {
    server_test_app()
  }

  runApp(
    shinyApp(ui = ui, server = server)
  )
}
