#' data_clean
#' @export data_clean
data_clean <- function(analysis_data) {
  box::use(dplyr)

  dose_num <- dplyr$filter(analysis_data, TreatmentNew == "Treatment")
  dose_num <- dplyr$select(dose_num, Dose)
  dose_num <- unlist(dose_num)
  dose_num <- unique(dose_num)

  final_data_num <- data.frame(Dose = dose_num, order = order(dose_num))

  analysis_data <- dplyr$left_join(analysis_data, final_data_num)
  analysis_data <- dplyr$mutate(analysis_data,
    TreatmentNew = as.character(TreatmentNew),
    TreatmentNew = dplyr$if_else(TreatmentNew == "Treatment",
      paste0("Dose ", order), TreatmentNew
    ),
    TreatmentNew = factor(TreatmentNew)
  )
  #
  analysis_data <- dplyr$select(analysis_data, -order)

  return(analysis_data)
}
