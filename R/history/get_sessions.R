#' get_sessions
#' @export get_sessions
get_sessions <- function() {
  con <- connect_table()
  on.exit(dbDisconnect(con))
  sessions <-
    tbl(con, "sessions") %>%
    dplyr$arrange(desc(timestamp)) %>%
    collect() %>%
    dplyr$mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"))
  sessions
}
