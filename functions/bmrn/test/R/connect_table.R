#' connect_table
#' @export connect_table
connect_table <- function(dir = "app.db") {
  # bd <- Sys.getenv("BASE_DIR")
  # pth <- path_join(c(bd, dir))
  conn <- dbConnect(RSQLite::SQLite(), dir)
}
