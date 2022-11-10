#' testSpinner
#' @export
testSpinner <- function(...) {
  box::use(shinycssloaders[withSpinner])
  withSpinner(..., hide.ui = FALSE)
}
