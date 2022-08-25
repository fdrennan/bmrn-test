#' transform_diagnostics
#' @export transform_diagnostics

transform_diagnostics <- function(analysis_data, baseline) {
  transformed <- transformation_check(analysis_data)
  transformed_data <- transformed$transformed
  power <- transformed$bc_transformation

  if (all(is.na(transformed_data$Baseline)) | baseline == FALSE) {
    var <- "Response_Transformed"
  } else {
    var <- "Response_Transformed_bc"
  }

  return(list(
    transformed_data = transformed_data,
    box_cox = power,
    variable = var,
    error_transform = transformed$error_transform
  ))
}
