#' ggplot_theme_test
#' @export ggplot_theme_test
ggplot_theme_test <- function() {
  theme <- test_theme_base()

  rebase <- unlist(theme$colors$rebase)
  base_size <- theme$fonts$size$h5
  axis_size <- theme$fonts$size$h5
  background_color <- theme$ndexr$background

  ggthemes::theme_gdocs() +
    ggplot2$theme(axis.text.x =ggplot2$(angle = 45, vjust = 0.5, hjust = 1))
}
