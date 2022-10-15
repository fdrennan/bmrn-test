#' link_btn
#' @export
link_btn <- function(id, label) {
  message("Deprecate me link_btn")
  div(actionButton(id, label), class = "link-btn")
}
