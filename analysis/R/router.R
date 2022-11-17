#' @export
router <- function(ns) {
  box::use(shiny.router)
  box::use(. / router)
  # box::use(./footer)
  box::use(. / ui_home)
  box::use(. / analysis_a_run)
  box::use(. / analysis_a_report)
  box::use(. / download_historical)
  box::use(. / analysis_a_session_setup)
  home <- shiny.router$route(path = "home", ui = ui_home$ui_home(ns("home")))
  export <- shiny.router$route(
    path = "export",
    ui = download_historical$ui_download_historical(ns("download_historical"))
  )
  analysisasetup <- shiny.router$route(
    path = "analysisasetup",
    ui = analysis_a_session_setup$test_session_setup(ns("test_session_setup"))
  )
  analysisa_run <- shiny.router$route(path = "analysisa_run", ui = analysis_a_run$analysis_a_run(ns("analysis_a_run")))
  report <- shiny.router$route(
    path = "report",
    ui = analysis_a_report$ui_analysis_a_report(ns("analysis_a_report"))
  )
  page_404 <- shiny.router$page404(message404 = "...hmmm")

  shiny.router$make_router(
    default = home,
    export,
    analysisasetup,
    analysisa_run,
    report,
    page_404 = page_404
  )
}
