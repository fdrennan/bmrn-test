#' connect_table
#' @export connect_table
connect_table <- function(dir = "./data/app.db") {
  box::use(DBI[dbConnect], RSQLite[SQLite])
  conn <- dbConnect(RSQLite::SQLite(), dir)
}
