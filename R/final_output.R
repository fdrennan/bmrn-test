#' final_output
#' @export final_output


final_output <- function(transformed_data, toi, emmeans_obj, final_contrast, power,
                         variable, save = "No") {
  final_contrast <- final_contrast %>%
    mutate(p.value = ifelse(p.value == 0, "< 0.001", p.value))
  ################################################################################
  # Generate Tables
  # Compute Summary statistic for the data on the original scale for both the
  # average and specific time point
  # Average Time
  AT_os <- transformed_data %>%
    group_by(TreatmentNew, Time) %>%
    summarize(sd = sd(Response)) %>%
    group_by(TreatmentNew) %>%
    summarize(se = mean(sd) / n()) %>%
    inner_join(transformed_data %>%
      group_by(TreatmentNew) %>%
      summarize(
        mean = mean(Response),
        median = median(Response)
      ), .)
  # Specific Time pointQ

  # Need to make sure that toi matches above
  ST_os <- transformed_data %>%
    filter(Time == toi) %>%
    group_by(TreatmentNew, Time) %>%
    summarize(
      mean = mean(Response),
      median = median(Response),
      se = sd(Response) / n()
    )

  os_together <- bind_rows(AT_os, ST_os) %>%
    arrange(TreatmentNew) %>%
    mutate(
      TreatmentNew = factor(TreatmentNew),
      Endpoint = if_else((row_number() %% 2) == 1,
        "Average", "Specific Time"
      )
    ) %>%
    select(TreatmentNew, Endpoint, mean, median, se) %>%
    arrange(TreatmentNew)
  # Back Transformation
  ST_bt <- emmeans_obj$ST %>%
    data.frame() %>%
    filter(Time == toi) %>%
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
  AT_bt <- emmeans_obj$AT %>%
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

  bt_together <- bind_rows(AT_bt, ST_bt) %>%
    arrange(TreatmentNew) %>%
    mutate(
      TreatmentNew = factor(TreatmentNew),
      Endpoint = if_else((row_number() %% 2) == 1,
        "Average", "Specific Time"
      )
    ) %>%
    select(TreatmentNew, Endpoint, emmean_bt, se_bt) %>%
    arrange(TreatmentNew)

  # LSmeans
  ST_lsmeans <- emmeans_obj$ST %>%
    data.frame() %>%
    filter(Time == toi)

  AT_lsmeans <- emmeans_obj$AT %>%
    data.frame()

  lsmeans_together <- bind_rows(AT_lsmeans, ST_lsmeans) %>%
    arrange(TreatmentNew) %>%
    mutate(
      TreatmentNew = factor(TreatmentNew),
      Endpoint = if_else((row_number() %% 2) == 1,
        "Average", "Specific Time"
      )
    ) %>%
    select(TreatmentNew, Endpoint, emmean, SE) %>%
    arrange(TreatmentNew) %>%
    rename(
      emmean_lsmeans = emmean,
      se_lsmeans = SE
    )

  # If power = 1 no transformation was conducted, otherwise we will need to add more
  # summary of the back transform data
  summary_stat <- os_together %>% inner_join(lsmeans_together)
  if (power != 1) {
    summary_stat <- summary_stat %>%
      inner_join(bt_together) %>%
      rename(
        "Back Transformed Mean" = emmean_bt,
        "Back Transformed SE" = se_bt
      ) %>%
      mutate_at(
        .vars = grep("Back Transformed", colnames(.), value = TRUE),
        .funs = ~ round(., 2)
      )
  }

  summary_stat <- summary_stat %>%
    data.frame() %>%
    mutate_at(.vars = grep("se", colnames(.)), .funs = ~ round(., 3)) %>%
    mutate_at(.vars = c("mean", "median", "emmean_lsmeans"), .funs = ~ round(., 2))
  tab1 <- table_1(final_contrast = final_contrast, os_together = summary_stat, toi = toi)
  tab2 <- table_2(final_contrast = final_contrast, os_together = summary_stat, toi = toi)
  tab3 <- table_3(final_contrast = final_contrast, os_together = summary_stat, toi = toi)
  empty_col <- tab1 %>% apply(2, function(a) sum(is.na(a)))
  tab1 <- tab1[, which(empty_col < nrow(tab1))] %>%
    rename(
      Treatment = TreatmentNew,
      "Original Scale Mean" = mean,
      "Original Scale Median" = median,
      "Original Scale SE" = se,
      "Transformed Scale Mean" = emmean_lsmeans,
      "Transformed Scale SE" = se_lsmeans
    ) %>%
    select(-grep("emmean|lsmean", colnames(.), value = TRUE))
  colnames(tab1) <- gsub("\\.", " ", colnames(tab1))

  empty_col <- tab2 %>% apply(2, function(a) sum(is.na(a)))
  tab2 <- tab2[, which(empty_col < nrow(tab2))] %>%
    rename(
      Treatment = TreatmentNew,
      "Original Scale Mean" = mean,
      "Original Scale Median" = median,
      "Original Scale SE" = se,
      "Transformed Scale Mean" = emmean_lsmeans,
      "Transformed Scale SE" = se_lsmeans
    ) %>%
    select(-grep("emmean|lsmean", colnames(.), value = TRUE))
  colnames(tab2) <- gsub("\\.", " ", colnames(tab2))
  empty_col <- tab3 %>% apply(2, function(a) sum(is.na(a)))
  tab3 <- tab3[, which(empty_col < nrow(tab3))] %>%
    rename(Treatment = TreatmentNew) %>%
    select(-grep("emmean|lsmean", colnames(.), value = TRUE))
  colnames(tab3) <- gsub("\\.", " ", colnames(tab3))

  return(list(tab1 = tab1, tab2 = tab2, tab3 = tab3, power = power))
}

#' html_tables
#' @export
html_tables <- function(transformed_data, tab_list) {
  trt_map <- distinct(transformed_data, Treatment, TreatmentNew)

  tab1 <- tab_list$tab1
  tab1 <- tab1[, which(apply(tab1, 2, function(a) !all(a == "")))]
  tab1 <- distinct(transformed_data, Treatment, TreatmentNew) %>%
    rename(
      "Treatment_orig" = Treatment,
      "Treatment" = TreatmentNew
    ) %>%
    inner_join(., tab1) %>%
    dplyr::select(-Treatment) %>%
    rename("Treatment" = Treatment_orig)

  tab2 <- tab_list$tab2
  tab2 <- tab2[, which(apply(tab2, 2, function(a) !all(a == "")))]
  tab2 <- distinct(transformed_data, Treatment, TreatmentNew) %>%
    rename(
      "Treatment_orig" = Treatment,
      "Treatment" = TreatmentNew
    ) %>%
    arrange(Treatment) %>%
    inner_join(., tab2) %>%
    dplyr::select(-Treatment) %>%
    rename("Treatment" = Treatment_orig)

  tab3 <- tab_list$tab3
  tab3 <- tab3[, which(apply(tab3, 2, function(a) !all(a == "")))]
  tab3 <- distinct(transformed_data, Treatment, TreatmentNew) %>%
    rename(
      "Treatment_orig" = Treatment,
      "Treatment" = TreatmentNew
    ) %>%
    inner_join(., tab3) %>%
    dplyr::select(-Treatment) %>%
    rename("Treatment" = Treatment_orig)

  swap_table <- transformed_data %>% distinct(Treatment, TreatmentNew)
  for (i in 1:nrow(swap_table)) {
    colnames(tab1) <- gsub(
      pattern = swap_table$TreatmentNew[i],
      replacement = swap_table$Treatment[i], x = colnames(tab1)
    )
    colnames(tab2) <- gsub(
      pattern = swap_table$TreatmentNew[i],
      replacement = swap_table$Treatment[i], x = colnames(tab2)
    )
    colnames(tab3) <- gsub(
      pattern = swap_table$TreatmentNew[i],
      replacement = swap_table$Treatment[i], x = colnames(tab3)
    )
  }

  return(list(tab1 = tab1, tab2 = tab2, tab3 = tab3))
}

#' transform_table
#' @export
transform_table <- function() {
  transform_table <- data.frame(
    power = c(2, 1, 0.5, 0, -0.5, -1),
    transform_name = c(
      "A Squared", "No transformation",
      "A Square Root", "A Log",
      "An Inverse Square Root",
      "An Inverse"
    )
  )
}
#' html_UI
#' @export

html_UI <- function(transformed_data, tables) {
  transform_table <- data.frame(
    power = c(2, 1, 0.5, 0, -0.5, -1),
    transform_name = c(
      "Squared", "Identity",
      "Square Root", "Log",
      "Inverse Square Root",
      "Inverse"
    )
  )

  tab1 <- tables$tab1
  tab2 <- tables$tab2
  tab3 <- tables$tab3

  colnames(tab1) <- gsub("p value", "p value", colnames(tab1))
  colnames(tab2) <- gsub("p value", "p value", colnames(tab2))
  colnames(tab3) <- gsub("p value", "p value", colnames(tab3))
  colnames(tab1) <- gsub("Difference", "Difference (95% CI)", colnames(tab1))
  colnames(tab2) <- gsub("Difference", "Difference (95% CI)", colnames(tab2))
  colnames(tab3) <- gsub("Difference", "Difference (95% CI)", colnames(tab3))

  transform <- 1

  footer <- if_else(substr(x = transform, start = 1, stop = 1) %in% c("A", "E", "I", "O", "U"),
    if_else(substr(x = transform, start = 1, stop = 2) == "Id",
      "No transformation was applied to the data. Difference and CI are estimated using model based LSmean.",
      paste(transform, "was applied to these data. Difference and CI are estimated using model based LSmean.")
    ),
    paste("A", transform, "was applied to these data")
  )
  if (!any(grepl("Transformed", colnames(tab1)))) {
    group <- unique(word(colnames(tab1)[6:ncol(tab1)], -1))
    group <- map_chr(.x = group, .f = ~ {
      tmp <- trt_map %>% filter(TreatmentNew == .x)
      as.character(tmp$Treatment)
    })
    colnames(tab1)[6:ncol(tab1)] <- gsub(" from.*", "", colnames(tab1)[6:ncol(tab1)])
    tab1HTML <- tableHTML(tab1,
      rownames = FALSE, spacing = "15px 15px",
      second_headers = list(
        c(2, 3, rep(2, length(group))),
        c("", "Summary Statistics", paste("vs.", group))
      ),
      caption = "Comparison between Controls and Wild Type",
      footer = footer
    )
  } else {
    group <- unique(word(colnames(tab1)[8:ncol(tab1)], -1))
    group <- map_chr(.x = group, .f = ~ {
      tmp <- trt_map %>% filter(TreatmentNew == .x)
      as.character(tmp$Treatment)
    })
    colnames(tab1)[8:ncol(tab1)] <- gsub(" from.*", "", colnames(tab1)[8:ncol(tab1)])
    tab1HTML <- tableHTML(tab1,
      rownames = FALSE, spacing = "15px 15px",
      second_headers = list(
        c(2, 3, 2, rep(2, length(group))),
        c(
          "", "Summary Statistics", "Transformed summary Statistics",
          paste("vs.", group)
        )
      ),
      caption = "Comparison between Controls and Wild Type",
      footer = footer
    )
  }

  if (!any(grepl("Transformed", colnames(tab2)))) {
    group <- c()
    for (i in seq(6, ncol(tab2), 2)) {
      group <- c(group, paste(word(colnames(tab2)[i], -c(2, 1)), collapse = " "))
    }
    group <- map_chr(.x = group, .f = ~ {
      tmp <- trt_map %>% filter(TreatmentNew == .x)
      as.character(tmp$Treatment)
    })
    colnames(tab2)[6:ncol(tab2)] <- gsub(" from.*", "", colnames(tab2)[6:ncol(tab2)])
    tab2HTML <- tableHTML(tab2,
      rownames = FALSE, spacing = "15px 15px",
      second_headers = list(
        c(2, 3, rep(2, length(group))),
        c("", "Summary Statistics", paste("vs.", group))
      ),
      caption = "Comparison between Treatments and Vehicle",
      footer = footer
    )
  } else {
    group <- c()
    for (i in seq(8, ncol(tab2), 2)) {
      group <- c(group, paste(word(colnames(tab2)[i], -c(2, 1)), collapse = " "))
    }
    group <- map_chr(.x = group, .f = ~ {
      tmp <- trt_map %>% filter(TreatmentNew == .x)
      as.character(tmp$Treatment)
    })
    colnames(tab2)[8:ncol(tab2)] <- gsub(" from.*", "", colnames(tab2)[8:ncol(tab2)])
    tab2HTML <- tableHTML(tab2,
      rownames = FALSE, spacing = "15px 15px",
      second_headers = list(
        c(2, 3, 2, rep(2, length(group))),
        c(
          "", "Summary Statistics", "Transformed summary Statistics",
          paste("vs.", group)
        )
      ),
      caption = "Comparison between Treatments and Vehicle",
      footer = footer
    )
  }

  group <- c()
  for (i in seq(3, ncol(tab3), 2)) {
    group <- c(group, paste(word(colnames(tab3)[i], -c(2, 1)), collapse = " "))
  }
  group <- map_chr(.x = group, .f = ~ {
    tmp <- trt_map %>% filter(TreatmentNew == .x)
    as.character(tmp$Treatment)
  })
  colnames(tab3)[3:ncol(tab3)] <- gsub(" from.*", "", colnames(tab3)[3:ncol(tab3)])
  tab3HTML <- tableHTML(tab3,
    rownames = FALSE, spacing = "15px 15px",
    second_headers = list(
      c(2, rep(2, length(group))),
      c("", paste("vs.", group))
    ),
    caption = "Comparison between Treatments and Controls/Wild Type",
    footer = footer
  )

  return(list(tab1HTML, tab2HTML, tab3HTML))
}

#' column_labels
#' @export
column_labels <- function(df_gt, column, label) {
  cols_list <- as.list(label) %>% purrr::set_names(column)

  df_gt %>%
    cols_label(.list = cols_list)
}

#' html_table_gt
#' @export

html_table_gt <- function(data, title, footer, include_summary, summary_only, transformation, analysis_type, endpoint) {
  data <- data %>%
    mutate_all(~ replace(., is.na(.), "")) %>%
    mutate(`Time Points` = if_else(grepl("Average", `Time Points`),
      "Overall Average",
      `Time Points`
    ))
  if (analysis_type == "Exploratory") {
    data <- data %>%
      mutate(num = as.numeric(gsub("[A-z]| ", "", `Time Points`))) %>%
      arrange(Treatment, num) %>%
      select(-num)
  }

  if (summary_only & transformation) {
    table_gt <- data %>%
      gt() %>%
      tab_header(
        title = title
      ) %>%
      tab_spanner(
        label = endpoint,
        columns = grep("Original", colnames(data), value = TRUE)
      ) %>%
      cols_label(
        `Original Scale Mean` = "Mean",
        `Original Scale Median` = "Median",
        `Original Scale SE` = "SE"
      )
  } else {
    data <- data %>% dplyr::select(-grep("Original", colnames(.), value = TRUE))

    if (!include_summary) {
      table_gt <- data %>%
        rowwise() %>%
        mutate_at(
          .vars = grep("Difference", colnames(.), value = TRUE),
          .funs = ~ gsub(" \\(", "<br>(", .)
        ) %>%
        gt() %>%
        tab_header(
          title = title
        ) %>%
        tab_source_note(source_note = footer)

      groups <- gsub(
        pattern = "Difference from ", replacement = "",
        x = grep("Difference", colnames(data), value = TRUE)
      )

      for (i in groups) {
        col1 <- paste0("Difference from ", i)
        col2 <- paste0("p value from ", i)
        table_gt <- table_gt %>%
          tab_spanner(
            label = paste("Difference from", i),
            columns = grep(pattern = i, x = colnames(data), value = TRUE)
          ) %>%
          column_labels(., col1, "LSMEAN Diff (95% CI)") %>%
          fmt_markdown(columns = everything()) %>%
          column_labels(., col2, "p value") %>%
          cols_align(
            align = "center",
            columns = everything()
          )
      }
    } else {
      table_gt <- data %>%
        rowwise() %>%
        mutate_at(
          .vars = grep("Difference", colnames(.), value = TRUE),
          .funs = ~ gsub(" \\(", "<br>(", .)
        ) %>%
        gt() %>%
        tab_header(
          title = title
        ) %>%
        tab_source_note(source_note = footer)



      if (transformation) {
        table_gt <- table_gt %>%
          tab_spanner(
            label = "Transformed Scale",
            columns = grep("Transformed Scale", colnames(data), value = TRUE)
          ) %>%
          cols_label(
            `Transformed Scale Mean` = "Mean",
            `Transformed Scale SE` = "SE"
          ) %>%
          tab_spanner(
            label = "Back Transformed",
            columns = grep("Back Transformed", colnames(data), value = TRUE)
          ) %>%
          cols_label(
            `Back Transformed Mean` = "Mean",
            `Back Transformed SE` = "SE"
          )
      } else {
        table_gt <- table_gt %>%
          tab_spanner(
            label = endpoint,
            columns = grep("Transformed Scale", colnames(data), value = TRUE)
          ) %>%
          cols_label(
            `Transformed Scale Mean` = "Mean",
            `Transformed Scale SE` = "SE"
          )
      }


      groups <- gsub(
        pattern = "Difference from ", replacement = "",
        x = grep("Difference", colnames(data), value = TRUE)
      )

      for (i in groups) {
        col1 <- paste0("Difference from ", i)
        col2 <- paste0("p value from ", i)
        table_gt <- table_gt %>%
          tab_spanner(
            label = paste("Difference from", i),
            columns = grep(pattern = i, x = colnames(data), value = TRUE)
          ) %>%
          column_labels(., col1, "LSMEAN Diff (95% CI)") %>%
          fmt_markdown(columns = everything()) %>%
          column_labels(., col2, "p value") %>%
          cols_align(
            align = "center",
            columns = everything()
          )
      }
    }
  }
  table_gt
}
