#' get_sessions
#' @export get_sessions
get_sessions <- function() {
  con <- connect_table()
  on.exit(dbDisconnect(con))
  sessions <-
    tbl(con, "sessions") %>%
    arrange(desc(timestamp)) %>%
    collect() %>%
    mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"))
  sessions
}
