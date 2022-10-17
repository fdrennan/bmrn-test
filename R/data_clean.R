#' data_clean
#' @export data_clean
data_clean <- function(analysis_data) {
  dose_num <- analysis_data %>%
    filter(TreatmentNew == "Treatment") %>%
    select(Dose) %>%
    unlist() %>%
    unique()

  final_data_num <- data.frame(Dose = dose_num, order = order(dose_num))

  analysis_data <- analysis_data %>%
    left_join(final_data_num) %>%
    mutate(
      TreatmentNew = as.character(TreatmentNew),
      TreatmentNew = if_else(TreatmentNew == "Treatment",
        paste0("Dose ", order), TreatmentNew
      ),
      TreatmentNew = factor(TreatmentNew)
    ) %>%
    select(-order)

  return(analysis_data)
}
