#' pre_modeling
#' @export
pre_modeling <- function(input_data, baseline) {
  box::use(. / data_clean)
  box::use(. / transform_diagnostics)
  box::use(dplyr)
  box::use(forcats)
  box::use(. / variance_check_basic)
  box::use(. / variance_check)
  box::use(furrr)
  box::use(. / final_model)
  box::use(stats)
  analysis_data <- data_clean$data_clean(input_data)
  times <- unique(analysis_data$Time)[
    order(as.numeric(gsub("[A-z]| ", "", unique(analysis_data$Time))))
  ]
  analysis_data <-
    dplyr$mutate(
      analysis_data,
      Time = forcats$as_factor(Time),
      TreatmentNew = droplevels(TreatmentNew)
    )
  analysis_data <- dplyr$arrange(analysis_data, TreatmentNew, SubjectID, Time)

  ready_final_model <- transform_diagnostics$transform_diagnostics(analysis_data, baseline)
  transformed_data <-
    variance_check_basic$variance_test_basic(
      transformed_data = ready_final_model$transformed_data,
      variable = ready_final_model$variable
    )

  ready_final_model$transformed_data <- transformed_data$data
  ready_final_model$error <- data.frame(
    error_trans = ready_final_model$error_transform,
    error_bm_var = transformed_data$error_bm_var
  )
  browser()
  debug(variance_check$variance_check)
  transformed_data_vc <-
    variance_check$variance_check(
      transformed_data = transformed_data$data,
      variable = ready_final_model$variable
    )
  transformed_data_vc <- dplyr$arrange(transformed_data_vc, TreatmentNew, SubjectID, Time)

  ready_final_model$transformed_data <- transformed_data_vc

  if (all(ready_final_model$error == FALSE)) {
    best_model <- furrr$future_map_dfr(.x = c("AR1", "ARH1", "CS", "CSH", "TOEP", "UN"), .f = ~ {
      tmp <- try(final_model$final_model(
        transformed_data = dplyr$filter(transformed_data_vc, basic_model),
        best = .x, var = ready_final_model$variable
      ))
      if (class(tmp) != "try-error") {
        return(data.frame(model = .x, AIC = stats$AIC(tmp)))
      }
    }, .options = furrr$furrr_options(seed = TRUE), .progress = TRUE)

    best_model <- unlist(best_model$model[which.min(best_model$AIC)])
    ready_final_model$best_model <- best_model
  } else {
    ready_final_model$best_model <- NULL
  }
  return(ready_final_model)
}
#'
