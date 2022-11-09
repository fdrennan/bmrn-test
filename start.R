

go <- function() {
  options(require_validation = FALSE)
  # devtools::load_all()
  options("cachetest" = FALSE)
  options("test_version" = "Version 1.22")
  if (isTRUE(getOption("production"))) {
    options(shiny.maxRequestSize = 300 * 1024^2)
    options(require_validation = TRUE)
    options(send = TRUE)
    options("cachetest" = FALSE)
    options("devmode" = FALSE)
  } else {
    options(require_validation = FALSE)
    options(send = FALSE)
    options("devmode" = TRUE)
  }
  box::use(./R/main)
  box::use(shiny)
  shiny$runApp(
    shiny$shinyApp(ui = main$ui_main, server = main$server_main)
  )
}

go()

if (FALSE) {
  rstudioapi::restartSession('source("start.R")')
}