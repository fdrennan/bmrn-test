#' test_analysis_card
#' @export test_analysis_card
test_analysis_card <- function(test_name = "Analysis A",
                               test_description = stringi::stri_rand_lipsum(1),
                               test_link = "analysis_a", ignore_dl = TRUE) {
  fluidRow(
    column(
      10,
      offset = 1,
      a(href = test_link, h1(test_name)),
      column(8, p(test_description))
    )
  )
}
