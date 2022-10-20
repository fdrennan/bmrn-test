#' ui_email
#' @export
ui_email <- function(id = "email") {
  ns <- NS(id)
  div(
    actionButton(ns("submit"), "Submit")
  )
}

#' server_email
#' @export
server_email <- function(id = "email") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        resp <- GET("http://apilink:8000/code/markdown?code=mtcars")
        # resp <- GET('http://apilink:8000/email?user.name=drennanfreddy@gmail.com&passwd=AeliaEmerson32&to=fr904103@bmrn.com')
        resp <- content(resp, "text")
      })
    }
  )
}
