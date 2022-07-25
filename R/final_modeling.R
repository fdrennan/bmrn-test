#' final_modeling
#' @export
final_modeling <- function(ready_final_model, toi=NULL, analysis_type) {
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

  if(analysis_type == 'Exploratory'){
   output_tables = map(.x = setNames(levels(transformed_data$Time),levels(transformed_data$Time)),
        .f =~{
          .x = unname(.x)
  contrast_list <- generate_contrasts(
    toi = .x,
    data = transformed_data,
    time_order = time_order,
    analysis_type = 'Exploratory'
  )

  contrasts_stats <- contrast_padjust(
    model = final_model,
    contrast_list = contrast_list,
    data = transformed_data,
    variable = var,
    analysis_type = 'Exploratory')

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
          variable = var,
          analysis_type = "Exploratory"
        )
      }
    )
   tab1 = map_dfr(.x = output_tables, .f = ~{.x$tab1 %>% dplyr::filter(!grepl('Average',`Times Included`))})
   tab2 = map_dfr(.x = output_tables, .f = ~{.x$tab2 %>% dplyr::filter(!grepl('Average',`Times Included`))})
   tab3 = map_dfr(.x = output_tables, .f = ~{.x$tab3 %>% dplyr::filter(!grepl('Average',`Times Included`))})
   output_tables = output_tables[[1]]
   output_tables$tab1 = tab1
   output_tables$tab2 = tab2
   output_tables$tab3 = tab3
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
      analysis_type = "Confirmatory"
    )

    output_tables <- final_output(
      transformed_data = transformed_data,
      toi = toi,
      emmeans_obj = contrasts_stats$emmeans_obj,
      final_contrast = contrasts_stats$final_contrast,
      power = power,
      variable = var,
      analysis_type = "Confirmatory"
    )
  }
  return(output_tables)
}
