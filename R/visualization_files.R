#' pre_modeling
#' @export
pre_modeling <- function(input_data, baseline) {
  analysis_data <- data_clean(input_data)
  times <- unique(analysis_data$Time)[
    order(as.numeric(gsub("[A-z]| ", "", unique(analysis_data$Time))))
  ]
  analysis_data <- analysis_data %>%
    mutate(
      Time = as_factor(Time),
      # TreatmentNew = factor(TreatmentNew,
      #   levels = c(
      #     "Wild Type", "Negative Control", "Positive Control",
      #     "Other Comparator", "Vehicle",
      #     grep("Dose", levels(TreatmentNew), value = TRUE)
      #   )
      #),
      TreatmentNew = droplevels(TreatmentNew)
    ) %>%
    arrange(TreatmentNew, SubjectID, Time)
  ready_final_model <- transform_diagnostics(analysis_data, baseline)
  transformed_data <-
    variance_test_basic(
      transformed_data = ready_final_model$transformed_data,
      variable = ready_final_model$variable
    )

  ready_final_model$transformed_data <- transformed_data$data
  ready_final_model$error <- data.frame(
    error_trans = ready_final_model$error_transform,
    error_bm_var = transformed_data$error_bm_var
  )

  transformed_data_vc <-
    variance_check(
      transformed_data = transformed_data$data,
      variable = ready_final_model$variable
    ) %>%
    arrange(TreatmentNew, SubjectID, Time)
  
  ready_final_model$transformed_data <- transformed_data_vc
  
  if(all(ready_final_model$error == FALSE)){
  best_model <- future_map_dfr(.x = c("AR1", "ARH1", "CS", "CSH", "TOEP", "UN"), .f = ~ {
    print(.x)
    tmp <- try(final_model(
      transformed_data = transformed_data_vc %>% filter(basic_model),
      best = .x, var = ready_final_model$variable
    ))
    if (class(tmp) != "try-error") {
      return(data.frame(model = .x, AIC = AIC(tmp)))
    }
  })
  best_model <- best_model$model[which.min(best_model$AIC)] %>% unlist()
  ready_final_model$best_model <- best_model
  }else{
    ready_final_model$best_model <- NULL
}
  return(ready_final_model)
}

#' quickplot
#' @export
quickplot <- function(ready_final_model) {
  transformed_data <- ready_final_model$transformed_data
  if (all(is.na(transformed_data$Baseline))) {
    var <- "Response_Transformed"
    plots <- vizualization(
      transformed_data = transformed_data,
      power = power
    )
  } else {
    var <- "Response_Transformed_bc"
    transformed_data <- transformed_data %>%
      mutate(Response_Transformed_bc = Response_Transformed - Baseline)
    plots <- vizualization_cb(
      transformed_data = transformed_data,
      power = power
    )
  }
}
