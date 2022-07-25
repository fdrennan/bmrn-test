# library(gt)
# library(tidyverse)
# library(glue)
#
# tab3 = read.csv('../Desktop/tab3.csv') %>% select(-X)
#
#
# tab3_gt = tab3 %>%
#   rowwise() %>%
#   mutate_at(.vars = grep('Difference', colnames(.), value = TRUE),
#             .funs = ~gsub(' \\(', "<br>(",.)) %>%
#   gt() %>%
#   tab_header(
#     title = "Comparison between Doses and Controls/Wild Type",
#     subtitle = "No transformation was applied to the data.
#     Difference and CI are estimated using model based LSmean") %>%
#   tab_spanner(
#     label = "Difference from Wild Type",
#     columns = grep('Wild', colnames(tab3), value = TRUE)) %>%
#   cols_label(
#     Difference.from.Wild.Type = html("LSMEAN Diff<br>(95% CI)"),
#     p.value.from.Wild.Type = "p value") %>%
#   fmt_markdown(columns = everything()) %>%
#   tab_spanner(
#     label = "Difference from Negative Control",
#     columns = grep('Negative', colnames(tab3), value = TRUE)) %>%
#   cols_label(
#     Difference.from.Negative.Control = html("LSMEAN Diff<br>(95% CI)"),
#     p.value.from.Negative.Control = "p value") %>%
#   fmt_markdown(columns = everything()) %>%
#   cols_align(
#     align = 'center',
#     columns = everything()
#   )
#
#
#
# if(any(grepl('Transformed',colnames(tab3)))){
#   tab3_gt = tab3_gt %>%
#     cols_label(
#       Transformed.Scale.Mean = "Mean",
#       Transformed.Scale.Median = "Median",
#       Transformed.Scale.SE = "SE")
# }
#
# tab3_gt
#
#
#
#
