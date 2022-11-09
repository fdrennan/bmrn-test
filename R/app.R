
#' @export
ui_app <- function(id = "app") {
  box::use(shiny)
  box::use(./router)
  box::use(./headers)
  box::use(bs4Dash)
  box::use(./navbar)
  box::use(./footer)
  box::use(./test_theme)
  ns <- shiny$NS(id)
  router <- router$router(ns)
  # ui_download_historical(ns('download_historical'))
  shiny$tags$div(
    class = "bg-light",
    bs4Dash$dashboardPage(
      fullscreen = FALSE, dark = FALSE,
      header = bs4Dash$dashboardHeader(shiny$tags$div(
        test_theme$headers(),
        navbar$ui_navbar(ns("navbar"))
      )),
      body = bs4Dash$dashboardBody(
        router$ui
      ),
      sidebar = bs4Dash$dashboardSidebar(disable = T),
      footer = bs4Dash$dashboardFooter(
        footer$ui_footer(ns("footer"))
      )
    )
  )
}

#' @export
server_app <- function(id = "app") {
  box::use(shiny)
  box::use(./router)
  box::use(./footer)
  box::use(./home)
  box::use(./navbar)
  box::use(./download_historical)
  box::use(./analysis_a_session_setup)
  # library(test)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      plan(multiprocess)
      ns <- session$ns
      router <- router$router(ns)
      router$server(input, output, session)
      footer$server_footer()
      home$server_home("home")
      navbar$server_navbar("navbar")
      download_historical$server_download_historical("download_historical")
      session_out <- test_session_setup_server("test_session_setup")
      analysis_a_session_setup$analysis_a_run_server("analysis_a_run", session_out, getOption("cachetest", FALSE))
    }
  )
}
