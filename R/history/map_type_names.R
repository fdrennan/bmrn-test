#' map_type_names
#' @export map_type_names
map_type_names <- function(type_names) {
  imap_dfr(
    type_names, function(x, y) {
      tibble(
        label = x,
        snake = y
      )
    }
  )
}
