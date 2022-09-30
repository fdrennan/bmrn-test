#' store_state
#' @export store_state
store_state <- function(input, id) {
  cli::cli_alert_info("Storing {id}")
  message(glue("Storing data {id}"))
  redisConnect(host = Sys.getenv("REDIS_HOST"))
  redisSet(id, reactiveValuesToList(input))
  redisClose()
}
