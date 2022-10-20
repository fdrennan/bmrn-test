#' data_clean
#' @export data_clean
data_clean <- function(analysis_data) {
  dose_num <- analysis_data %>%
    filter(Treatment == "Treatment") %>%
    select(Dose) %>%
    unlist() %>%
    unique()

  final_data_num <- data.frame(Dose = dose_num, order = order(dose_num))

  analysis_data <- analysis_data %>%
    left_join(final_data_num) %>%
    mutate(
      Treatment = as.character(Treatment),
      Treatment = if_else(Treatment == "Treatment",
        paste0("Dose ", order), Treatment
      ),
      Treatment = factor(Treatment)
    ) %>%
    select(-order)

  return(analysis_data)
}
