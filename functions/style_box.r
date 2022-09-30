#' @export
style_box <- function(dir = "functions", collapse = TRUE) {
  box::use(styler)
  r_file_paths <- list.files(dir, full.names = T, recursive = T, pattern = ".r$")
  out <- lapply(r_file_paths[[1]], function(pth) {
    styler$style_file(pth)
    file_lines <- readLines(pth)
    is_function <- cumsum(grepl("function", file_lines))
    lapply(
      split(file_lines, is_function),
      function(fn) {
        is_box <- grepl("box::use", fn)
        print(fn[is_function])
        print(fn[is_box])
      }
    )
  })
}

style_box()
