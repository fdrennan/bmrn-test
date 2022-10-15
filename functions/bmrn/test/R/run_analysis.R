#' run_analysis
#' @export run_analysis
run_analysis <- function(analysis_data) {
  analysis_data <- analysis_data %>%
    rename(Int_Time = colnames(.)[6]) %>%
    # Would be nice to have this not hard coded in case there are changes to template
    mutate(
      basic_model = if_else(grepl("treatment|Vehicle", name),
        "Yes", "No"
      ),
      Response_diff = as.numeric(Response) - as.numeric(Int_Time),
      Time = as.factor(Time)
    )


  dose_num <- analysis_data %>%
    filter(name == "treatment") %>%
    select(Dose) %>%
    unlist() %>%
    unique()

  final_data_num <- data.frame(Dose = dose_num, order = order(dose_num))

  analysis_data <- analysis_data %>%
    left_join(final_data_num) %>%
    mutate(
      name = as.character(name),
      name = if_else(name == "treatment",
        paste0("Dose ", order), name
      ),
      name = factor(name)
    ) %>%
    select(-c(order, value)) %>%
    rename(
      Treatment_input = Treatment,
      Treatment = name
    )



  # Running my functions

  transformed <- transformation_check(analysis_data)
  transformed_data <- transformed$transformed
  power <- transformed$bc_transformation
  transformed_data <- variance_test_basic(transformed_data)
  best_model <- cor_select(transformed_data)
  transformed_data <- variance_check(transformed_data)
  model <- final_model(transformed_data = transformed_data, best = best_model)
  toi <- transformed_data$timeSelection[1]
  contrast_list <- generate_contrasts(
    toi = toi,
    data = transformed_data
  )
  if (FALSE) {
    est <- emmeans(object = model, ~ Treatment * Time, adjust = "none")
    est_me <- emmeans(object = model, ~Treatment, adjust = "none")
    final_contrast <- future_map_df(.x = LETTERS[1:9], .f = ~ {
      print(.x)
      # Maybe there is a way to have NA for contrasts that don't exist
      out <- final_contrasts(model = model, cont_list = contrast_list[[.x]], est = est)
      if (!is.null(out)) {
        num_pairs <- nrow(out) / 2
        if (.x == "G") {
          if (num_pairs == 1) {
            rownames(out) <- c("G12", "G12_st")
          } else {
            rownames(out) <- cbind(combn(1:num_pairs, 2), combn(1:num_pairs, 2)) %>%
              t() %>%
              data.frame() %>%
              arrange(X1, X2) %>%
              mutate(final = case_when(
                row_number() %% 2 == 1 ~ paste0("G", X1, X2),
                row_number() %% 2 == 0 ~ paste0("G", X1, X2, "_st")
              )) %>%
              select(final) %>%
              unlist()
          }
        } else if (num_pairs == 1) {
          rownames(out) <- c(.x, paste0(.x, "_st"))
        } else {
          rownames(out) <- c(
            paste(.x, 1:num_pairs, sep = ""),
            paste(.x, 1:num_pairs, "_st",
              sep = ""
            )
          )
        }
        return(out %>% select(-contrast))
      }
    })
  }


  ################################################################################
  # make some plots
  if (power != 1) {
    orig_tran_scale <- transformed_data %>%
      pivot_longer(
        cols = c("Response_diff", "Response_Transformed"),
        names_to = "scale", values_to = "Difference"
      )
    bar_plot <- ggplot(data = orig_tran_scale, aes(x = Time, y = Difference)) +
      geom_boxplot(aes(color = Treatment), show.legend = FALSE) +
      geom_jitter(width = 0.1, aes(color = Treatment), show.legend = FALSE) +
      facet_grid(scale ~ Treatment, scales = "free_y") +
      theme_bw()

    orig_tran_scale_sum <- orig_tran_scale %>%
      group_by(Treatment, Time, scale) %>%
      summarize(Difference = mean(Difference))

    line_plot <- ggplot(data = orig_tran_scale_sum, aes(x = Time, y = Difference, group = Treatment)) +
      geom_line(aes(color = Treatment, linetype = Treatment), size = 1.5) +
      facet_wrap(scale ~ ., scales = "free_y") +
      theme_bw() +
      theme(legend.position = "bottom")
  } else {
    bar_plot <- ggplot(data = transformed_data, aes(x = Time, y = Response_diff)) +
      geom_boxplot(aes(color = Treatment), show.legend = FALSE) +
      geom_jitter(width = 0.1, aes(color = Treatment), show.legend = FALSE) +
      facet_wrap(Treatment ~ ., nrow = 1) +
      theme_bw()

    transformed_data_sum <- transformed_data %>%
      group_by(Treatment, Time) %>%
      summarize(Response_diff = mean(Response_diff))

    line_plot <- ggplot(data = transformed_data_sum, aes(x = Time, y = Response_diff, group = Treatment)) +
      geom_line(aes(color = Treatment, linetype = Treatment), size = 1.5) +
      theme_bw() +
      theme(legend.position = "bottom")
  }

  # ggplotly(line_plot) #Interactive plots
  # ggplotly(bar_plot) #Interactive plots
  # #Combine plots into one
  combined_plot <- ggarrange(
    plotlist = list(bar_plot, line_plot),
    common.legend = TRUE, legend = "bottom"
  )
  # Has not been implemted yet
  return(combined_plot)

  if (FALSE) {
    ################################################################################
    # Generate Tables

    # Compute Summary statistic for the data on the original scale for both the
    # average and specific time point
    # Average Time
    AT_os <- analysis_data %>%
      group_by(Treatment, Time) %>%
      summarize(sd = sd(Response_diff)) %>%
      group_by(Treatment) %>%
      summarize(se = mean(sd) / n()) %>%
      inner_join(analysis_data %>%
        group_by(Treatment) %>%
        summarize(
          mean = mean(Response_diff),
          median = median(Response_diff)
        ), .)
    # Specific Time point
    # Need to make sure that toi matches above
    ST_os <- analysis_data %>%
      filter(grepl("Week6", Time)) %>%
      group_by(Treatment, Time) %>%
      summarize(
        mean = mean(Response_diff),
        median = median(Response_diff),
        se = sd(Response_diff) / n()
      )
    os_together <- plyr::rbind.fill(AT_os, ST_os) %>%
      arrange(Treatment) %>%
      mutate(
        Treatment = factor(Treatment),
        Endpoint = if_else((row_number() %% 2) == 1,
          "Average", "Specific Time"
        )
      ) %>%
      select(Treatment, Endpoint, mean, median, se) %>%
      arrange(Treatment)
    # Back Transformation
    ST_bt <- est %>%
      data.frame() %>%
      filter(grepl("Week6", Time)) %>%
      mutate(
        emmean_bt = case_when(
          power == 0 ~ exp(emmean),
          !(power %in% c(0, 1)) ~ emmean^(1 / power),
          power == 1 ~ NaN
        ),
        se_bt = case_when(
          power == 1 ~ NaN,
          power == 0 ~ exp(emmean) * SE,
          !(power %in% c(0, 1)) ~ (1 / power) * emmean^(1 / power - 1) * SE
        ),
      )
    AT_bt <- emmeans(object = model, ~Treatment, adjust = "none") %>%
      data.frame() %>%
      mutate(
        emmean_bt = case_when(
          power == 0 ~ exp(emmean),
          !(power %in% c(0, 1)) ~ emmean^(1 / power),
          power == 1 ~ NaN
        ),
        se_bt = case_when(
          power == 1 ~ NaN,
          power == 0 ~ exp(emmean) * SE,
          !(power %in% c(0, 1)) ~ (1 / power) * emmean^(1 / power - 1) * SE
        ),
      )
    bt_together <- plyr::rbind.fill(AT_bt, ST_bt) %>%
      arrange(Treatment) %>%
      mutate(
        Treatment = factor(Treatment),
        Endpoint = if_else((row_number() %% 2) == 1,
          "Average", "Specific Time"
        )
      ) %>%
      select(Treatment, Endpoint, emmean_bt, se_bt) %>%
      arrange(Treatment)
    # LSmeans
    ST_lsmeans <- est %>%
      data.frame() %>%
      filter(grepl("Week6", Time))
    AT_lsmeans <- emmeans(object = model, ~Treatment, adjust = "none") %>%
      data.frame()
    lsmeans_together <- plyr::rbind.fill(AT_lsmeans, ST_lsmeans) %>%
      arrange(Treatment) %>%
      mutate(
        Treatment = factor(Treatment),
        Endpoint = if_else((row_number() %% 2) == 1,
          "Average", "Specific Time"
        )
      ) %>%
      select(Treatment, Endpoint, emmean, SE) %>%
      arrange(Treatment) %>%
      rename(
        emmean_lsmeans = emmean,
        se_lsmeans = SE
      )
    # If power = 1 no transformation was conducted, otherwise we will need to add more
    # summary of the back transform data
    summary_stat <- os_together %>% inner_join(lsmeans_together)
    if (power != 1) {
      summary_stat <- summary_stat %>% inner_join(bt_together)
    }

    source("R/final_tables.R")
    tab1 <- table_1(final_contrast = final_contrast, os_together = summary_stat)
    tab2 <- table_2(final_contrast = final_contrast, os_together = summary_stat)
    tab3 <- table_3(final_contrast = final_contrast, os_together = summary_stat)

    wb <- createWorkbook()
    addWorksheet(wb = wb, sheetName = "Table 1")
    addWorksheet(wb = wb, sheetName = "Table 2")
    addWorksheet(wb = wb, sheetName = "Table 3")
    writeData(wb = wb, sheet = "Table 1", x = tab1)
    writeData(wb = wb, sheet = "Table 2", x = tab2)
    writeData(wb = wb, sheet = "Table 3", x = tab3)
  }
  # saveWorkbook(wb, file = 'Example.xlsx', overwrite = TRUE)
}
