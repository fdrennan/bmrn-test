options(require_validation = FALSE)
# library(test)
# devtools::document()
# devtools::load_all()
plan(multiprocess)
options('cachetest' = FALSE)
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

start_app()
