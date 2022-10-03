

# print(a)
#' @export
style_box <- function(dir = "functions", collapse = TRUE) {

  box::use(styler, dplyr, stringr, purrr, tidytext)
  r_file_paths <- list.files(dir, full.names = T, recursive = T, pattern = ".r$")
  out <- purrr$imap_dfr(r_file_paths, function(pth, nm) {
    styler$style_file(pth)
    data <- readLines(pth)
    out <- dplyr$tibble(
      row = seq_along(data),
      file_lines = data
    ) |>
      dplyr$mutate(
        box_start = grepl("box::use", file_lines),
        box_group = cumsum(box_start)
      )

    out <-
      out |>
      tidytext$unnest_tokens(
        word,
        file_lines,
        strip_punct = FALSE,
        to_lower = FALSE
      ) |>
      dplyr$group_by(box_group) |>
      dplyr$mutate(
        box_section = cumprod(!grepl(")", word) * 1),
        box_section = box_section * box_group
      )

    out <- dplyr$filter(
      out, box_section > 0,
      !word %in% c("box", "use", ":", "(", ")")
    ) |>
      dplyr$mutate(is_package = dplyr$lead(word=='['))

    # out
  })
}

data <- style_box()
data
# print(data)
