#' ui_admin
#' @export
ui_admin <- function(id = "admin") {
  ns <- NS(id)
  # tableOutput(ns('table'))
}

#' server_admin
#' @export
server_admin <- function(id = "admin") {
  moduleServer(
    id,
    function(input, output, session) {
      output$table <- renderTable({
        con <- connect_table()
        tbl(con, "sessions") %>% collect()
      })
    }
  )
}
