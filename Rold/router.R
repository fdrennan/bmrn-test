#' @export
router <- function(ns) {
  make_router(
    # route("home", ui_home(ns("home"))),
    # route(
    #   "analysisasetup",
    #   test_session_setup(ns("test_session_setup"))
    # ),
    route("analysisa_run", analysis_a_run(ns("analysis_a_run"))),
    route("report", ui_analysis_a_report(ns("analysis_a_report"))),
    page_404 = page404(message404 = "...hmmm")
  )
}
