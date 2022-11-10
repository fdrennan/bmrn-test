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
  shiny.router$make_router(
    shiny.router$route("home", ui_home$ui_home(ns("home"))),
    shiny.router$route("export", download_historical$ui_download_historical(ns("download_historical"))),
    shiny.router$route(
      "analysisasetup",
      analysis_a_session_setup$test_session_setup(ns("test_session_setup"))
    ),
    shiny.router$route("analysisa_run", analysis_a_run$analysis_a_run(ns("analysis_a_run"))),
    shiny.router$route("report", . / analysis_a_report$ui_analysis_a_report(ns("analysis_a_report"))),
    page_404 = shiny.router$page404(message404 = "...hmmm")
  )
}
