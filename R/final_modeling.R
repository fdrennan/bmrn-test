#' final_modeling
#' @export
final_modeling <- function(ready_final_model, toi = NULL, analysis_type, overall_trend = FALSE) {
  var <- ready_final_model$var
  power <- ready_final_model$box_cox
  transformed_data <- ready_final_model$transformed_data %>%
    arrange(TreatmentNew, SubjectID, Time)
  best_model <- ready_final_model$best_model
  time_order <- unique(transformed_data$Time)
  final_model <- final_model(
    transformed_data = transformed_data,
    best = best_model, variable = var
  )

  if (analysis_type == "Exploratory") {

    output_tables <- future_map(#.progress = TRUE, .options = furrr_options(seed = 123),
      .x = setNames(levels(transformed_data$Time), levels(transformed_data$Time)),
      .f = ~ {
        
        print('futuremap start')
        .x <- unname(.x)
        print(.x)
        contrast_list <- generate_contrasts(
          toi = .x,
          data = transformed_data,
          time_order = time_order,
          analysis_type = "Exploratory"
        )

        contrasts_stats <- contrast_padjust(
          model = final_model,
          contrast_list = contrast_list,
          data = transformed_data,
          variable = var,
          analysis_type = "Exploratory"
        )


        output_tables <- final_output(
          transformed_data = transformed_data,
          toi = .x,
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
      tab1 <- bind_rows(tab1, output_tables[[i]]$tab1 %>% mutate_all(~ as.character(.))) %>% dplyr::filter(!grepl("Average", `Time Points`))
      tab2 <- bind_rows(tab2, output_tables[[i]]$tab2 %>% mutate_all(~ as.character(.))) %>% dplyr::filter(!grepl("Average", `Time Points`))
      tab3 <- bind_rows(tab3, output_tables[[i]]$tab3 %>% mutate_all(~ as.character(.))) %>% dplyr::filter(!grepl("Average", `Time Points`))
    }
    output_tables <- output_tables[[1]]
    output_tables$tab1 <- tab1
    output_tables$tab2 <- tab2
    output_tables$tab3 <- tab3
  } else {
    contrast_list <- generate_contrasts(
      toi = toi,
      data = transformed_data,
      time_order = time_order,
      analysis_type = "Confirmatory"
    )

    contrasts_stats <- contrast_padjust(
      model = final_model,
      contrast_list = contrast_list,
      data = transformed_data,
      variable = var,
      analysis_type = "Confirmatory",
      overall_trend = overall_trend
    )
    output_tables <- final_output(
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
    tab1 <- output_tables$tab1 %>%
      mutate_all(~ as.character(.)) %>%
      dplyr::filter(!grepl("Average", `Time Points`))
    tab2 <- output_tables$tab2 %>%
      mutate_all(~ as.character(.)) %>%
      dplyr::filter(!grepl("Average", `Time Points`))
    tab3 <- output_tables$tab3 %>%
      mutate_all(~ as.character(.)) %>%
      dplyr::filter(!grepl("Average", `Time Points`))
    output_tables$tab1 <- tab1
    output_tables$tab2 <- tab2
    output_tables$tab3 <- tab3
  }
  return(output_tables)
}
