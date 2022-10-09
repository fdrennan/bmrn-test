#' @export
return_nfl_data <- function(schedules = 2019:2019, game_type = "SB") {
  box::use(nflfastR, dplyr)
  future::plan("multisession")
  ids <- nflfastR$fast_scraper_schedules(schedules)
  ids
}

#' @export
ui_pigskin_analytics <- function(id = "pigskin_analytics") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("app"))
}

#' @export
server_pigskin_analytics <- function(id = "pigskin_analytics") {
  box::use(shiny)
  box::use(. / nfl)
  box::use(. / utilities / datatable)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      nfl_data <- shiny$reactive(nfl$return_nfl_data())
      shiny$observe({
        shiny$req(nfl_data())
        output$app <- shiny$renderUI({
          datatable$ui_dt(id = ns("nfl_data"))
        })
        datatable$server_dt(id = "nfl_data", data = nfl_data())
      })
    }
  )
}
