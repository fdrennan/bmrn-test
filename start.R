options(require_validation = FALSE)

library(test)
devtools::document()
devtools::install()
devtools::load_all()
library(shinyjs)
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

runApp(
  shinyApp(ui = ui, server = server)
)
