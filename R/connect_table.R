#' connect_table
#' @export connect_table
connect_table <- function(dir = "app.db") {
  box::use(DBI[dbConnect], RSQLite[SQLite])
  # pth <- path_join(c(bd, dir))
  conn <- dbConnect(RSQLite::SQLite(), dir)
}
