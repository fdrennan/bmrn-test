#' #' quickplot
#' #' @export
#' quickplot <- function(ready_final_model) {
#'   transformed_data <- ready_final_model$transformed_data
#'   if (all(is.na(transformed_data$Baseline))) {
#'     var <- "Response_Transformed"
#'     plots <- vizualization(
#'       transformed_data = transformed_data,
#'       power = power
#'     )
#'   } else {
#'     var <- "Response_Transformed_bc"
#'     transformed_data <- transformed_data %>%
#'       dplyr$mutate(Response_Transformed_bc = Response_Transformed - Baseline)
#'     plots <- vizualization_cb(
#'       transformed_data = transformed_data,
#'       power = power
#'     )
#'   }
#' }
