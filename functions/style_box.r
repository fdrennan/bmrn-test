#' @export
style_box <- function(dir = "functions", collapse = TRUE) {
  box::use(styler, dplyr, stringr, purrr)
  r_file_paths <- list.files(dir, full.names = T, recursive = T, pattern = ".r$")
  out <- purrr$map_dfr(r_file_paths, function(pth) {
    styler$style_file(pth)
    file_lines <- readLines(pth)
    out <- dplyr$tibble(
      file_lines = file_lines,
      is_function = grepl("function", file_lines),
      function_group = cumsum(is_function),
      is_box = grepl("box::use", file_lines),
      is_close = grepl(")", file_lines)
    ) |>
      dplyr$tibble() |>
      dplyr$mutate(
        code_groups = dplyr$case_when(
          is_box & is_close ~ "one line",
          is_box ~ "start",
          dplyr$lead(is_box & is_close) ~ as.character("end")
        )
      )
    if (nrow(out) == 1) {
      return(out)
    }
    # if (nrow(out) == 81) browser()
    code_seg <- out |>
      dplyr$pull(code_groups)

    for (i in seq_along(code_seg)) {
      prev_value <- code_seg[i - 1]
      cur_value <- code_seg[i]
      next_value <- code_seg[i + 1]
      print(glue::glue("prev: {prev_value}, cur: {cur_value}, next_value: {next_value}"))
      code_seg[i + 1] <- dplyr$case_when(
        length(cur_value) == 0 ~ next_value,
        is.na(cur_value) & next_value == "end" ~ NA_character_,
        !is.na(next_value) ~ next_value,
        cur_value == "start" & is.na(next_value) ~ "start"
      )
    }
    out$code_seg <- code_seg[1:nrow(out)]
    out
  })
  out
}

a <- style_box()
