#' @export
start_app <- function() {
  plan(multiprocess)
  options("test_version" = "Version 1.21")
  runApp(
    shinyApp(ui = ui, server = server)
  )
}
