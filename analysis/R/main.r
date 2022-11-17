

#' @export
ui_main <- function() {
  box::use(. / app)
  app$ui_app("app")
}




#' @export
server_main <- function(input, output, session) {
  box::use(. / app)
  app$server_app("app")
}
