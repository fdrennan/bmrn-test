#' update_state
#' @export update_state
update_state <- function(input, user) {
  box::use(shiny[is.reactivevalues, reactiveValuesToList])

  if (is.reactivevalues(input)) {
    input <- reactiveValuesToList(input)
  }
  if (length(input)) {
    box::use(. / connections / sqlite, storr)
    con <- sqlite$connection_sqlite(getOption("ndexr_sqlite_path"))
    st <- storr$storr_dbi("tblData", "tblKeys", con)

    state <- update.list(state, input)
    state$redis_timestamp <- Sys.time()
    state$user_name <- user
    con <- mongo("user_history", "redpul", Sys.getenv("MONGO_URI"))
    con$insert(state)
    cli_alert_info("input")
    print(toJSON(input, pretty = TRUE))
    cli_alert_info("state")
    print(toJSON(state, pretty = TRUE))
    redisSet(user, state)
    redisClose()
  }
}
