options(require_validation = FALSE)
# library(test)

devtools::load_all()
plan(multiprocess)
options("test_version" = "Version 1.133")
if (isTRUE(getOption("production"))) {
  options(require_valiation = TRUE)
  Sys.setenv(BASE_DOMAIN = "/qsci/test")
  options(send = TRUE)
  options("devmode" = FALSE)
} else {
  options(send = FALSE)
  options("devmode" = FALSE)
}


shinyApp(ui = ui, server = server)
