#' final_modeling
#' @export
final_modeling <- function(ready_final_model, toi = NULL, analysis_type, overall_trend = FALSE) {
  {
    box::use(dplyr)
    box::use(furrr)
    box::use(purrr)
    box::use(stats)
    box::use(. / generate_contrasts)
    box::use(. / final_model)
    box::use(. / contrast_padjust)
    box::use(. / final_output)
    box::use(. / final_modeling)
  }

  var <- ready_final_model$var
  power <- ready_final_model$box_cox
  transformed_data <- dplyr$arrange(ready_final_model$transformed_data, TreatmentNew, SubjectID, Time)
  best_model <- ready_final_model$best_model
  time_order <- unique(transformed_data$Time)
  final_model <- final_model$final_model(
    transformed_data = transformed_data,
    best = best_model, variable = var
  )

  if (analysis_type == "Exploratory") {
    output_tables <- purrr$map(
      # .progress = TRUE, .options = furrr$furrr_options(seed = TRUE),
      .x = stats$setNames(levels(transformed_data$Time), levels(transformed_data$Time)),
      .f = function(x) {
        print("futuremap start")
        x <- unname(x)
        contrast_list <- generate_contrasts$generate_contrasts(
          toi = x,
          data = transformed_data,
          time_order = time_order,
          analysis_type = "Exploratory"
        )

        contrasts_stats <- contrast_padjust$contrast_padjust(
          model = final_model,
          contrast_list = contrast_list,
          data = transformed_data,
          variable = var,
          analysis_type = "Exploratory"
        )

        output_tables <- final_output$final_output(
          transformed_data = transformed_data,
          toi = x,
          emmeans_obj = contrasts_stats$emmeans_obj,
          final_contrast = contrasts_stats$final_contrast,
          power = power,
          variable = var
        )
      }
    )

    tab1 <- data.frame()
    tab2 <- data.frame()
    tab3 <- data.frame()
    for (i in 1:length(output_tables)) {
      tab1 <- dplyr$bind_rows(tab1, final_modeling$clean_table(output_tables[[i]]$tab1))
      tab2 <- dplyr$bind_rows(tab2, final_modeling$clean_table(output_tables[[i]]$tab3))
      tab3 <- dplyr$bind_rows(tab3, final_modeling$clean_table(output_tables[[i]]$tab3))
    }
    output_tables <- output_tables[[1]]
    output_tables$tab1 <- tab1
    output_tables$tab2 <- tab2
    output_tables$tab3 <- tab3
  } else {
    contrast_list <- generate_contrasts$generate_contrasts(
      toi = toi,
      data = transformed_data,
      time_order = time_order,
      analysis_type = "Confirmatory"
    )

    contrasts_stats <- contrast_padjust$contrast_padjust(
      model = final_model,
      contrast_list = contrast_list,
      data = transformed_data,
      variable = var,
      analysis_type = "Confirmatory",
      overall_trend = overall_trend
    )
    output_tables <- final_output$final_output(
      transformed_data = transformed_data,
      toi = toi,
      emmeans_obj = contrasts_stats$emmeans_obj,
      final_contrast = contrasts_stats$final_contrast,
      power = power,
      variable = var
    )
  }
  if (!overall_trend) {
    # Remove when we include the overall average time
    tab1 <- dplyr$mutate_all(output_tables$tab1, ~ as.character(.))
    tab1 <- dplyr$filter(tab1, !grepl("Average", `Time Points`))

    tab2 <- dplyr$mutate_all(output_tables$tab2, ~ as.character(.))
    tab2 <- dplyr$filter(tab2, !grepl("Average", `Time Points`))

    tab3 <- dplyr$mutate_all(output_tables$tab3, ~ as.character(.))
    tab3 <- dplyr$filter(tab3, !grepl("Average", `Time Points`))

    output_tables$tab1 <- tab1
    output_tables$tab2 <- tab2
    output_tables$tab3 <- tab3
  }
  return(output_tables)
}


#' @export
clean_table <- function(data) {
  box::use(dplyr)
  data <- dplyr$mutate_all(data, ~ as.character(.))
  data <- dplyr$filter(data, !grepl("Average", `Time Points`))
  data
}
