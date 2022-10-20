#' @export
start_app <- function() {
  plan(multiprocess)
  options("test_version" = "Version 1.21")
  
  options('cachetest' = TRUE)
  if (isTRUE(getOption("production"))) {
    options(require_valiation = TRUE)
    Sys.setenv(BASE_DOMAIN = "/qsci/test")
    options(send = TRUE)
    options("devmode" = FALSE)
  } else {
    options(send = FALSE)
    options("devmode" = FALSE)
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
