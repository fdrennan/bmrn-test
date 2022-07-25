# library(gt)
# library(tidyverse)
# library(glue)
#
# tab1 = read.csv('../Desktop/tab1.csv') %>% select(-X)
#
#
# tab1_gt = tab1 %>%
#   rowwise() %>%
#   mutate_at(.vars = grep('Difference', colnames(.), value = TRUE),
#             .funs = ~gsub(' \\(', "<br>(",.)) %>%
#   gt() %>%
#   tab_header(
#     title = "Comparison between Controls and Wild Type",
#     subtitle = "No transformation was applied to the data.
#     Difference and CI are estimated using model based LSmean") %>%
#   tab_spanner(
#     label = "Original Scale",
#     columns = grep('Original', colnames(tab1), value = TRUE)) %>%
#   tab_spanner(
#     label = "Transformed Scale",
#     columns = grep('Transformed', colnames(tab1), value = TRUE)) %>%
#   cols_label(
#     Original.Scale.Mean = "Mean",
#     Original.Scale.Median = "Median",
#     Original.Scale.SE = "SE",
#     Times.Included = 'Times Points Included') %>%
#   tab_spanner(
#     label = "Difference from Vehicle",
#     columns = grep('Vehicle', colnames(tab1), value = TRUE)) %>%
#   cols_label(
#     Difference.from.Vehicle = html("LSMEAN Diff<br>(95% CI)"),
#     p.value.from.Vehicle = "p value") %>%
#   fmt_markdown(columns = everything()) %>%
#   cols_align(
#     align = 'center',
#     columns = everything()
#   )
#
#
#
# if(any(grepl('Transformed',colnames(tab1)))){
# tab1_gt = tab1_gt %>%
#   cols_label(
#     Transformed.Scale.Mean = "Mean",
#     Transformed.Scale.Median = "Median",
#     Transformed.Scale.SE = "SE")
# }
#
# tab1_gt
#
#
#
#
