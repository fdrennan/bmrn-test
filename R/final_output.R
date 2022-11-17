#' final_output
#' @export
final_output <- function(transformed_data, toi, emmeans_obj, final_contrast, power,
                         variable, save = "No") {
  {
    box::use(dplyr)
    box::use(stats)
    box::use(. / final_tables)
  }
  final_contrast <- dplyr$mutate(final_contrast, p.value = ifelse(p.value == 0, "< 0.001", p.value))
  ################################################################################
  # Generate Tables
  # Compute Summary statistic for the data on the original scale for both the
  # average and specific time point
  # Average Time
  atos_join <- dplyr$group_by(transformed_data, TreatmentNew)
  atos_join <- dplyr$summarize(
    atos_join,
    mean = mean(Response),
    median = stats$median(Response)
  )
  AT_os <- dplyr$group_by(transformed_data, TreatmentNew, Time)
  AT_os <- dplyr$summarize(AT_os, sd = stats$sd(Response))
  AT_os <- dplyr$group_by(AT_os, TreatmentNew)
  AT_os <- dplyr$summarize(AT_os, se = mean(sd) / dplyr$n())
  AT_os <- dplyr$inner_join(atos_join, AT_os)
  # Specific Time pointQ

  # Need to make sure that toi matches above
  ST_os <- dplyr$filter(transformed_data, Time == toi)
  ST_os <- dplyr$group_by(ST_os, TreatmentNew, Time)
  ST_os <- dplyr$summarize(ST_os,
    mean = mean(Response),
    median = stats$median(Response),
    se = stats$sd(Response) / dplyr$n()
  )

  os_together <- dplyr$bind_rows(AT_os, ST_os)
  os_together <- dplyr$arrange(os_together, TreatmentNew)
  os_together <- dplyr$mutate(
    os_together,
    TreatmentNew = factor(TreatmentNew),
    Endpoint = dplyr$if_else((dplyr$row_number() %% 2) == 1,
      "Average", "Specific Time"
    )
  )
  os_together <- dplyr$select(os_together, TreatmentNew, Endpoint, mean, median, se)
  os_together <- dplyr$arrange(os_together, TreatmentNew)
  # Back Transformation
  ST_bt <- data.frame(emmeans_obj$ST)
  ST_bt <- dplyr$filter(ST_bt, Time == toi)
  ST_bt <- dplyr$mutate(
    ST_bt,
    emmean_bt = dplyr$case_when(
      power == 0 ~ exp(emmean),
      !(power %in% c(0, 1)) ~ emmean^(1 / power),
      power == 1 ~ NaN
    ),
    se_bt = dplyr$case_when(
      power == 1 ~ NaN,
      power == 0 ~ exp(emmean) * SE,
      !(power %in% c(0, 1)) ~ (1 / power) * emmean^(1 / power - 1) * SE
    ),
  )
  AT_bt <- data.frame(emmeans_obj$AT)
  AT_bt <- dplyr$mutate(AT_bt,
    emmean_bt = dplyr$case_when(
      power == 0 ~ exp(emmean),
      !(power %in% c(0, 1)) ~ emmean^(1 / power),
      power == 1 ~ NaN
    ),
    se_bt = dplyr$case_when(
      power == 1 ~ NaN,
      power == 0 ~ exp(emmean) * SE,
      !(power %in% c(0, 1)) ~ (1 / power) * emmean^(1 / power - 1) * SE
    ),
  )

  bt_together <- dplyr$bind_rows(AT_bt, ST_bt)
  bt_together <- dplyr$arrange(bt_together, TreatmentNew)
  bt_together <- dplyr$mutate(
    bt_together,
    TreatmentNew = factor(TreatmentNew),
    Endpoint = dplyr$if_else((dplyr$row_number() %% 2) == 1,
      "Average", "Specific Time"
    )
  )
  bt_together <- dplyr$select(bt_together, TreatmentNew, Endpoint, emmean_bt, se_bt)
  bt_together <- dplyr$arrange(bt_together, TreatmentNew)

  # LSmeans
  ST_lsmeans <- data.frame(emmeans_obj$ST)
  ST_lsmeans <- dplyr$filter(ST_lsmeans, Time == toi)

  AT_lsmeans <- data.frame(emmeans_obj$AT)

  lsmeans_together <- dplyr$bind_rows(AT_lsmeans, ST_lsmeans)
  lsmeans_together <- dplyr$arrange(lsmeans_together, TreatmentNew)
  lsmeans_together <- dplyr$mutate(lsmeans_together,
    TreatmentNew = factor(TreatmentNew),
    Endpoint = dplyr$if_else((dplyr$row_number() %% 2) == 1,
      "Average", "Specific Time"
    )
  )
  lsmeans_together <- dplyr$select(
    lsmeans_together,
    TreatmentNew, Endpoint, emmean, SE
  )
  lsmeans_together <- dplyr$arrange(lsmeans_together, TreatmentNew)
  lsmeans_together <- dplyr$rename(lsmeans_together,
    emmean_lsmeans = emmean,
    se_lsmeans = SE
  )

  # If power = 1 no transformation was conducted, otherwise we will need to add more
  # summary of the back transform data
  summary_stat <- dplyr$inner_join(os_together, lsmeans_together)
  if (power != 1) {
    summary_stat <- dplyr$inner_join(summary_stat, bt_together)
    summary_stat <- dplyr$rename(
      summary_stat,
      "Back Transformed Mean" = emmean_bt,
      "Back Transformed SE" = se_bt
    )
    summary_stat <- dplyr$mutate_at(
      summary_stat,
      .vars = grep("Back Transformed", colnames(summary_stat), value = TRUE),
      .funs = ~ round(., 2)
    )
  }

  summary_stat <- data.frame(summary_stat)
  summary_stat <- dplyr$mutate_at(summary_stat, .vars = grep("se", colnames(summary_stat)), .funs = ~ round(., 3))
  summary_stat <- dplyr$mutate_at(summary_stat, .vars = c("mean", "median", "emmean_lsmeans"), .funs = ~ round(., 2))

  tab1 <- final_tables$table_1(final_contrast = final_contrast, os_together = summary_stat, toi = toi)
  tab2 <- final_tables$table_2(final_contrast = final_contrast, os_together = summary_stat, toi = toi)
  tab3 <- final_tables$table_3(final_contrast = final_contrast, os_together = summary_stat, toi = toi)
  empty_col <- apply(tab1, 2, function(a) sum(is.na(a)))
  tab1 <-
    dplyr$rename(
      tab1[, which(empty_col < nrow(tab1))],
      Treatment = TreatmentNew,
      # Remove this if the summary statistics are not included in the final table 3
      "Original Scale Mean" = mean,
      "Original Scale Median" = median,
      "Original Scale SE" = se,
      "Transformed Scale Mean" = emmean_lsmeans,
      "Transformed Scale SE" = se_lsmeans
    )
  tab1 <- dplyr$select(tab1, -grep("emmean|lsmean", colnames(tab1), value = TRUE))
  colnames(tab1) <- gsub("\\.", " ", colnames(tab1))

  empty_col <- apply(tab2, 2, function(a) sum(is.na(a)))
  tab2 <-
    dplyr$rename(
      tab2[, which(empty_col < nrow(tab2))],
      Treatment = TreatmentNew,
      "Original Scale Mean" = mean,
      "Original Scale Median" = median,
      "Original Scale SE" = se,
      "Transformed Scale Mean" = emmean_lsmeans,
      "Transformed Scale SE" = se_lsmeans
    )
  tab2 <- dplyr$select(tab2, -grep("emmean|lsmean", colnames(tab2), value = TRUE))
  colnames(tab2) <- gsub("\\.", " ", colnames(tab2))
  empty_col <- apply(tab3, 2, function(a) sum(is.na(a)))
  tab3 <-
    dplyr$rename(
      tab3[, which(empty_col < nrow(tab3))],
      Treatment = TreatmentNew,
      "Original Scale Mean" = mean,
      "Original Scale Median" = median,
      "Original Scale SE" = se,
      "Transformed Scale Mean" = emmean_lsmeans,
      "Transformed Scale SE" = se_lsmeans
    )

  tab3 <- dplyr$select(tab3, -grep("emmean|lsmean", colnames(tab3), value = TRUE))
  colnames(tab3) <- gsub("\\.", " ", colnames(tab3))

  return(list(tab1 = tab1, tab2 = tab2, tab3 = tab3, power = power))
}

#' html_tables
#' @export
html_tables <- function(transformed_data, tab_list) {
  box::use(dplyr)
  trt_map <- dplyr$distinct(transformed_data, Treatment, TreatmentNew)

  tab1 <- tab_list$tab1
  tab_join <- tab1[, which(apply(tab1, 2, function(a) !all(a == "")))]
  tab1 <- dplyr$distinct(transformed_data, Treatment, TreatmentNew)
  tab1 <- dplyr$rename(tab1,
    "Treatment_orig" = Treatment,
    "Treatment" = TreatmentNew
  )
  tab1 <- dplyr$inner_join(tab1, tab_join)
  tab1 <- dplyr$select(tab1, -Treatment)
  tab1 <- dplyr$rename(tab1, "Treatment" = Treatment_orig)

  tab2 <- tab_list$tab2
  tab2_join <- tab2[, which(apply(tab2, 2, function(a) !all(a == "")))]
  tab2 <- dplyr$distinct(transformed_data, Treatment, TreatmentNew)
  tab2 <- dplyr$rename(
    tab2,
    "Treatment_orig" = Treatment,
    "Treatment" = TreatmentNew
  )
  tab2 <- dplyr$arrange(tab2, Treatment)
  tab2 <- dplyr$inner_join(tab2, tab2_join)
  tab2 <- dplyr$select(tab2, -Treatment)
  tab2 <- dplyr$rename(tab2, "Treatment" = Treatment_orig)


  tab3 <- tab_list$tab3
  tab3_join <- tab3[, which(apply(tab3, 2, function(a) !all(a == "")))]
  tab3 <- dplyr$distinct(transformed_data, Treatment, TreatmentNew)
  tab3 <- dplyr$rename(tab3,
    "Treatment_orig" = Treatment,
    "Treatment" = TreatmentNew
  )
  tab3 <- dplyr$inner_join(tab3, tab3_join)
  tab3 <- dplyr$select(tab3, -Treatment)
  tab3 <- dplyr$rename(tab3, "Treatment" = Treatment_orig)

  swap_table <- dplyr$distinct(transformed_data, Treatment, TreatmentNew)
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
  box::use(dplyr)
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

  footer <- dplyr$if_else(substr(x = transform, start = 1, stop = 1) %in% c("A", "E", "I", "O", "U"),
    dplyr$if_else(substr(x = transform, start = 1, stop = 2) == "Id",
      "No transformation was applied to the data. Mean and SE are estimated using model based LSmean.",
      paste(transform, "was applied to these data. Mean and SE are estimated using model based LSmean.")
    ),
    paste("A", transform, "was applied to these data")
  )
  if (!any(grepl("Transformed", colnames(tab1)))) {
    group <- unique(word(colnames(tab1)[6:ncol(tab1)], -1))
    group <- map_chr(.x = group, .f = ~ {
      tmp <- dplyr$filter(trt_map, TreatmentNew == .x)
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
      tmp <- dplyr$filter(trt_map, TreatmentNew == .x)
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
      tmp <- dplyr$filter(trt_map, TreatmentNew == .x)
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
      tmp <- dplyr$filter(trt_map, TreatmentNew == .x)
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
    tmp <- dplyr$filter(trt_map, TreatmentNew == .x)
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
  box::use(purrr, cli, gt)
  cli$cli_alert("column_labels")
  cols_list <- purrr$set_names(as.list(label), column)

  gt$cols_label(df_gt, .list = cols_list)
}

#' html_table_gt
#' @export

html_table_gt <- function(data, title, footer, include_summary, summary_only, transformation, analysis_type, endpoint) {
  {
    box::use(dplyr)
    box::use(uuid)
    box::use(gt)
    box::use(. / final_output)
  }
  data <- dplyr$mutate_all(data, ~ replace(., is.na(.), ""))

  data <- dplyr$mutate(data,
    `Time Points` = dplyr$if_else(grepl("Average", `Time Points`),
      "Overall Average",
      `Time Points`
    )
  )

  if (summary_only & transformation) {
    table_gt <- gt$gt(data)
    table_gt <- gt$tab_header(
      table_gt,
      title = title
    )
    table_gt <-
      gt$tab_spanner(
        table_gt,
        id = uuid$UUIDgenerate(),
        label = endpoint,
        columns = grep("Original", colnames(data), value = TRUE)
      )
    table_gt <- gt$cols_label(table_gt,
      `Original Scale Mean` = "Mean",
      `Original Scale Median` = "Median",
      `Original Scale SE` = "SE"
    )
  } else {
    data <- dplyr$select(data, -grep("Original", colnames(data), value = TRUE))

    if (!include_summary) {
      table_gt <- dplyr$rowwise(data)
      table_gt <- dplyr$mutate_at(
        table_gt,
        .vars = grep("Difference", colnames(table_gt), value = TRUE),
        .funs = ~ gsub(" \\(", "<br>(", .)
      )
      table_gt <- gt$gt(table_gt)
      table_gt <- gt$tab_header(
        table_gt,
        title = title
      )
      table_gt <- gt$tab_source_note(table_gt, source_note = footer)

      groups <- gsub(
        pattern = "Difference from ", replacement = "",
        x = grep("Difference", colnames(data), value = TRUE)
      )

      for (i in groups) {
        col1 <- paste0("Difference from ", i)
        col2 <- paste0("p value from ", i)
        table_gt <-
          gt$tab_spanner(
            table_gt,
            id = uuid$UUIDgenerate(),
            label = paste("Difference from", i),
            columns = grep(pattern = i, x = colnames(data), value = TRUE)
          )
        table_gt <- final_output$column_labels(table_gt, col1, "LSMEAN Diff (95% CI)")
        table_gt <- gt$fmt_markdown(table_gt, columns = dplyr$everything())
        table_gt <- final_output$column_labels(table_gt, col2, "p value")
        table_gt <- gt$cols_align(
          table_gt,
          align = "center",
          columns = dplyr$everything()
        )
      }
    } else {
      table_gt <- dplyr$rowwise(data)
      table_gt <-
        dplyr$mutate_at(table_gt,
          .vars = grep("Difference", colnames(table_gt), value = TRUE),
          .funs = ~ gsub(" \\(", "<br>(", .)
        )
      table_gt <- gt$gt(table_gt)
      table_gt <- gt$tab_header(table_gt,
        title = title
      )
      table_gt <- gt$tab_source_note(table_gt, source_note = footer)



      if (transformation) {
        table_gt <-
          gt$tab_spanner(table_gt,
            id = uuid$UUIDgenerate(),
            label = "Transformed Scale",
            columns = grep("Transformed Scale", colnames(data), value = TRUE)
          )
        table_gt <- gt$cols_label(table_gt,
          `Transformed Scale Mean` = "Mean",
          `Transformed Scale SE` = "SE"
        )
        table_gt <- gt$tab_spanner(table_gt,
          id = uuid$UUIDgenerate(),
          label = "Back Transformed",
          columns = grep("Back Transformed", colnames(data), value = TRUE)
        )
        table_gt <- gt$cols_label(table_gt,
          `Back Transformed Mean` = "Mean",
          `Back Transformed SE` = "SE"
        )
      } else {
        table_gt <-
          gt$tab_spanner(table_gt,
            id = uuid$UUIDgenerate(),
            label = endpoint,
            columns = grep("Transformed Scale", colnames(data), value = TRUE)
          )
        table_gt <- gt$cols_label(table_gt,
          `Transformed Scale Mean` = "Mean",
          `Transformed Scale SE` = "SE"
        )
      }


      groups <- gsub(
        pattern = "Difference from ", replacement = "",
        x = grep("Difference", colnames(data), value = TRUE)
      )

      j <- 0
      for (i in groups) {
        j <- j + 1
        col1 <- paste0("Difference from ", i)
        col2 <- paste0("p value from ", i)
        table_gt <-
          gt$tab_spanner(table_gt,
            id = uuid$UUIDgenerate(),
            label = paste("Difference from", i),
            columns = grep(pattern = i, x = colnames(data), value = TRUE)
          )

        table_gt <- final_output$column_labels(table_gt, col1, "LSMEAN Diff (95% CI)")
        table_gt <- gt$fmt_markdown(table_gt, columns = dplyr$everything())
        table_gt <- final_output$column_labels(table_gt, col2, "p value")
        table_gt <- gt$cols_align(table_gt,
          align = "center",
          columns = dplyr$everything()
        )

        table_gt
      }
    }
  }
  table_gt
}
