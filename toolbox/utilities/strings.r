#' @export
  str_detect_any <- function(string, find) {
    box::use(purrr)
    box::use(stringr)
    # print(length(string))
    out <- purrr$map_lgl(
      string, function(y) any(purrr$map_lgl(find, function(x) stringr$str_detect(y, x)))
    )
  }

