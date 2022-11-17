#' router_test
#' @export
router_test <- function() {
  make_router(
    route("home", ui_home()),
    route(
      "analysisasetup",
      analysis_a_session_setup(user = "testuser", is_admin = TRUE)
    ),
    route("analysisa_run", analysis_a_run(id = "test_1")),
    route("report", ui_analysis_a_report()),
    page_404 = page404(message404 = "...hmmm")
  )
}
