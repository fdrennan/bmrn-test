# library(gt)
# library(tidyverse)
# library(glue)
#
# tab2 = read.csv('../Desktop/tab2.csv') %>% select(-X)
#
#
# tab2_gt = tab2 %>%
#   mutate_all(.funs = ~ ifelse(is.na(.), "", .)) %>%
#  # rowwise() %>%
#  #mutate_at(.vars = grep('Difference', colnames(.), value = TRUE),
# #            .funs = ~gsub(' \\(', "<br>(",.)) %>%
#   gt() %>%
#   tab_header(
#     title = "Comparison between Doses") %>%
#   tab_source_note(
#     source_note ="No transformation was applied to the data.
#     Difference and CI are estimated using model based LSmean"
#   ) %>%
#   tab_spanner(
#     label = "Original Scale",
#     columns = grep('Original', colnames(tab2), value = TRUE)) %>%
#   tab_spanner(
#     label = "Transformed Scale",
#     columns = grep('Transformed', colnames(tab2), value = TRUE)) %>%
#   cols_label(
#     Original.Scale.Mean = "Mean",
#     Original.Scale.Median = "Median",
#     Original.Scale.SE = "SE",
#     Times.Included = 'Time Points Included') %>%
#   cols_align(
#     align = 'center',
#     columns = everything()
#   )
#
#
# fun <- function(df_gt, column, label)
# {
#   cols_list = as.list(label) %>% purrr::set_names(column)
#
#   df_gt %>%
#     cols_label(.list = cols_list)
# }
#
#
#
# number_of_doses = length(grep('Difference', colnames(tab2)))
# for(i in 1:number_of_doses){
#   col1 = paste0('Difference.from.Dose.',i)
#   col2 = paste0('p.value.from.Dose.',i)
#
#   print(c(col1,col2))
#   tab2_gt = tab2_gt %>%
#   tab_spanner(
#     label = paste("Difference from Dose", i),
#     columns = grep(pattern = i, x = colnames(tab2), value = TRUE)) %>%
#     fun(.,c(col1, col2), c(html("LSMEAN Diff<br>(95% CI)"), 'p value')) %>%
#     fmt_markdown(columns = everything())
# }
#
#
#
# if(any(grepl('Transformed',colnames(tab2)))){
#   tab2_gt = tab2_gt %>%
#     cols_label(
#       Transformed.Scale.Mean = "Mean",
#       Transformed.Scale.Median = "Median",
#       Transformed.Scale.SE = "SE")
# }
#
# tab2_gt
#
#
#
#
