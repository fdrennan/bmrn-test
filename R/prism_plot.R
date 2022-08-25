#' is.outlier
#' @export

is.outlier <- function(x) {
  x < quantile(x, .25) - 1.5 * IQR(x) |
    x > quantile(x, .75) + 1.5 * IQR(x)
}

#' prism_plot
#' @export
prism_plot <- function(data, tables, trt_sel,
                       time_sel, endpoint, format, cfb, power,
                       box_width = 2, axis_title_size = 30, axis_text_size = 24,
                       top_height = 2, bottom_height = 3, num_groups, type = "box",
                       inputs = NULL) {
  orig_groups <- levels(data$Treatment)
  colors <- c(
    ggprism_data$colour_palettes$floral,
    ggprism_data$colour_palettes$pastel
  )[1:length(orig_groups)]

  y_axis <- inputs$y_axisPrism
  tab1 <- tables$tab1
  tab2 <- tables$tab2
  tab3 <- tables$tab3

  trans_name <- transform_table()


  trans_name <- trans_name$transform_name[which(trans_name$power == power)]
  trans_name <- gsub("A ", "", trans_name)
  trans_name <- gsub("An ", "", trans_name)

  if (power == 1 | y_axis == "no_transform") {
    ylabel <- endpoint
    data <- data %>%
      mutate(Response_Transformed = Response)
    ylabel <- endpoint
  }

  if (y_axis == "transform" & power != 1) {
    ylabel <- paste(trans_name, endpoint)
    var <- "Response_Transformed"
  }


  if (y_axis == "change_from_baseline" & power == 1) {
    data <- data %>%
      mutate(Response_Transformed = Response_Transformed_bc)
    ylabel <- paste0("Change from Baseline \n", endpoint)
  }

  if (y_axis == "change_from_baseline" & power != 1) {
    data <- data %>%
      mutate(Response_Transformed = Response_Transformed_bc)
    ylabel <- paste0(trans_name, "\n Change from Baseline ", endpoint)
  }

  p_vals <- bind_rows(tab1, tab2, tab3) %>%
    select(Treatment, `Time Points`, grep("p value from", colnames(.))) %>%
    mutate_at(.vars = 3:ncol(.), .funs = ~ as.character(.)) %>%
    pivot_longer(
      cols = 3:ncol(.),
      names_to = "group2",
      values_to = "p value"
    )

  p_vals <-
    p_vals %>%
    rename(group1 = Treatment) %>%
    filter(
      complete.cases(.),
      `Time Points` != "Average Over Time",
      `p value` != ""
    )
  p_vals <-
    p_vals %>%
    mutate(
      group2 = gsub("p value from ", "", group2),
      `p value` = if_else(`p value` == "< 0.001", "0.001", `p value`),
      `p value` = as.numeric(`p value`),
      sig = case_when(
        `p value` > 0.05 ~ "ns",
        `p value` <= 0.05 & `p value` > 0.01 ~ "*",
        `p value` <= 0.01 &
          `p value` > 0.001 ~ "**",
        `p value` <= 0.001 &
          `p value` > 0.0001 ~ "***",
        `p value` < 0.001 ~ "****"
      )
    ) %>%
    filter(`p value` < 0.05) %>%
    filter(`Time Points` == time_sel) %>%
    arrange(group2, group1) %>%
    mutate(y.position = seq(
      1.55 * max(data$Response_Transformed),
      2.5 * max(data$Response_Transformed),
      length.out = nrow(.)
    )) %>%
    filter(
      group1 %in% trt_sel,
      group2 %in% trt_sel
    ) %>%
    mutate(new_y.position = y.position + 1 * row_number())



  correct_level_order <- data %>%
    arrange(TreatmentNew) %>%
    distinct(Treatment, TreatmentNew) %>%
    dplyr::select(Treatment) %>%
    unlist()


  data <- data %>%
    filter(
      Time == time_sel,
      Treatment %in% trt_sel
    ) %>%
    group_by(Treatment) %>%
    mutate(
      outlier = is.outlier(Response_Transformed),
      Treatment = factor(Treatment, levels = correct_level_order)
    ) %>%
    ungroup()

  data_max <- data %>%
    group_by(Treatment) %>%
    summarize(
      max = max(Response_Transformed),
      Mean_Response = mean(Response_Transformed),
      sd_Response = sd(Response_Transformed)
    ) %>%
    rename(Response_Transformed = Mean_Response) %>%
    mutate(
      error = if_else(Response_Transformed < 0, Response_Transformed - sd_Response, Response_Transformed + sd_Response),
      ymin = if_else(Response_Transformed < 0, error, Response_Transformed),
      ymax = if_else(Response_Transformed < 0, Response_Transformed, error)
    )

  if (type == "box") {
    full_prism <- ggplot(
      data %>% rename(group1 = Treatment),
      aes(x = group1, y = Response_Transformed, color = group1)
    ) +
      stat_boxplot(
        geom = "errorbar",
        width = 3 * length(unique(data$Treatment)) / num_groups^2,
        lwd = 1
      ) +
      geom_boxplot(
        aes(fill = group1),
        outlier.color = NA,
        lwd = ifelse(format == "word", 1, 2),
        fatten = 1,
        width = 3 * length(unique(data$Treatment)) / num_groups^2,
        alpha = 0.5
      ) +
      geom_jitter(
        # data = data %>% filter(outlier) %>% rename(group1 = Treatment),
        # plot outliers only
        # aes(shape = group1),
        size = ifelse(format == "word", 1.5, 3),
        position = position_dodge(width = 0.2)
      ) +
      stat_summary(
        fun = "mean",
        color = "black",
        size = ifelse(format == "word", 0.2, 0.5),
        show.legend = FALSE
      ) +
      scale_y_continuous(limits = c(NA, 3 * max(data$Response_Transformed))) +
      scale_fill_manual(values = colors, breaks = orig_groups) +
      scale_color_manual(values = colors, breaks = orig_groups) +
      guides(y = "prism_offset_minor") +
      theme_prism(base_size = ifelse(format == "word", 16, inputs$fontSize)) +
      # , palette = inputs$palette) +
      theme(legend.position = "none") +
      ylab(ylabel) +
      xlab("Treatment")

    bottom <- full_prism +
      scale_y_continuous(
        limits = c(1.1 * min(0, min(data$Response_Transformed)), 1.1 * max(data$Response_Transformed)),
        expand = expansion(mult = c(0, 0))
      ) +
      theme(
        plot.margin = margin(
          t = -10,
          r = 0,
          b = 0,
          l = 0
        ),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
      )
  } else {
    full_prism <- ggplot(
      data_max %>% rename(group1 = Treatment),
      aes(x = group1, y = Response_Transformed, color = group1)
    ) +
      geom_bar(
        aes(fill = group1),
        lwd = ifelse(format == "word", 1, 2),
        stat = "identity",
        width = 3 * length(unique(data$Treatment)) / num_groups^2,
        position = position_dodge(width = 0.7),
        alpha = 0.5
      ) +
      geom_jitter(
        data = data %>% rename(group1 = Treatment),
        aes(y = Response_Transformed),
        size = ifelse(format == "word", 1.5, 3),
        position = position_dodge(width = 0.2)
      ) +
      # stat_summary(
      #   data = data %>% rename(group1 = Treatment),
      #   fun = "mean",
      #   color = "black",
      #   show.legend = FALSE,
      #   ifelse(format == 'word', 1.5,3),
      # ) +
      geom_errorbar(
        aes(ymin = ymin, ymax = ymax),
        position = position_dodge(width = 0.7), width = 3 * length(unique(data$Treatment)) / num_groups^2,
        size = ifelse(format == "word", 1, 2)
      ) +
      guides(y = "prism_offset_minor") +
      scale_y_continuous(limits = c(NA, 1.1 * max(p_vals$new_y.position))) +
      scale_fill_manual(values = colors, breaks = orig_groups) +
      scale_color_manual(values = colors, breaks = orig_groups) +
      theme_prism(base_size = ifelse(format == "word", 16, inputs$fontSize)) +
      # , palette = inputs$palette) +
      theme(legend.position = "none") +
      ylab(ylabel) +
      xlab("Treatment")
    bottom <- full_prism +
      theme(
        plot.margin = margin(
          t = -10,
          r = 0,
          b = 0,
          l = 0
        ),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
      ) +
      scale_y_continuous(
        limits = c(1.1 * min(0, min(data$Response_Transformed)), 1.1 * max(data$Response_Transformed)),
        expand = expansion(mult = c(0, 0))
      )
  }
  if (format == "word") {
    bottom <- bottom + theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10)
    )
  }
  if (nrow(p_vals) > 0 & ((as.logical(cfb) == TRUE & y_axis == "change_from_baseline") |
    (as.logical(cfb) != TRUE & y_axis != "change_from_baseline"))) {
    full_prism <- full_prism + add_pvalue(
      data = p_vals,
      y.position = "new_y.position",
      label = "{sig}",
      tip.length = 0.02,
      label.size = ifelse(format == "word", 4, 8),
      color = "black",
      size = 2,
      step.increase = ifelse(format == "word", 0.02, 0.05)
    )

    top <- full_prism +
      scale_y_continuous(
        limits = c(0.9 * min(p_vals$new_y.position), NA),
        expand = expansion(mult = c(0, 0.1))
      )

    if (format == "word") {
      top <- top +
        theme(
          line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()
        ) +
        theme(plot.margin = margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0
        ))
    } else {
      top <- full_prism +
        scale_y_continuous(
          limits = c(0.9 * min(p_vals$new_y.position), NA),
          expand = expansion(mult = c(0.25, 0.1))
        ) +
        theme(
          line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()
        ) +
        theme(plot.margin = margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0
        ))
    }
    layout <- rbind(1, 2, 2)
    if (nrow(p_vals) > 6 & nrow(p_vals) < 12) {
      layout <- rbind(1, 2)
    }
    if (nrow(p_vals) >= 12) {
      layout <- rbind(1, 1, 2)
    }

    if (type == "box") {
      top <- top + ggtitle(paste("Box plot for Treatment Groups at", time_sel))
      combined <- grid.arrange(grobs = list(top, bottom), layout_matrix = layout)
    } else {
      top <- top + ggtitle(paste("Bar Chart for Treatment Groups at", time_sel))
      combined <- grid.arrange(grobs = list(top, bottom), layout_matrix = layout)
    }

    return(combined)
  } else {
    if (type == "box") {
      bottom <- bottom +
        ggtitle(paste("Box Plot for Treatment Groups at", time_sel)) +
        theme(
          plot.margin = margin(
            t = 0,
            r = 0,
            b = 0,
            l = 0
          ),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
        )
    } else {
      bottom <- bottom +
        ggtitle(paste("Bar Chart for Treatment Groups at", time_sel)) +
        theme(
          plot.margin = margin(
            t = 0,
            r = 0,
            b = 0,
            l = 0
          ),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
        )
      return(bottom)
    }
  }
}
