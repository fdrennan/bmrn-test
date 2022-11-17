

go <- function() {
  options(shiny.maxRequestSize = 300 * 1024^2)
  options("cachetest" = TRUE)
  box::use(. / R / main)
  box::use(shiny)
  library(grid)
  library(gridExtra)
  library(lattice)
  library(ggprism)
  shiny$runApp(
    shiny$shinyApp(ui = main$ui_main, server = main$server_main)
  )
}

go()

if (FALSE) {
  rstudioapi::restartSession('source("start.R")')
}
