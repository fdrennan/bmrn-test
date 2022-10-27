#' update_state
#' @export update_state
update_state <- function(input, user) {
  #
  if (is.reactivevalues(input)) {
    input <- reactiveValuesToList(input)
  }
  # cli_alert_info('Storing state for {user}')

  # names_input <- names(input)
  #
  # names_input <- names_input[rowSums(map_dfc(exclude_names, ~ str_detect(names_input, .))) == 0]
  # input <- map(input, function(inpt) {
  #   purrr::discard(inpt, ~ length(.) > 5)
  # })
  #
  # input <- purrr::keep(input, ~ length(.) <= len)
  if (length(input)) {
    con <- connect_table()
    if (inherits(con, "try-error")) {
      cli_alert_danger("Failed to store state")
      return()
    }
    #
    # state <- redisGet(user)
    # state <- update.list(state, input)
    # state$user_name <- user
    con <- mongo("user_history", "redpul", Sys.getenv("MONGO_URI"))
    con$insert(state)
    redisSet(user, state)
    redisClose()
  }
}
