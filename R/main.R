#' @export ui
ui <- function() {
  ui_app("app")
}

#' @export
server <- function(input, output, session) {
  server_app("app")
}
