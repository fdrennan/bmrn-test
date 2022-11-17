#' is.outlier
#' @export
is.outlier <- function(x) {
  box::use(stats)
  x < stats$quantile(x, .25) - 1.5 * stats$IQR(x) |
    x > stats$quantile(x, .75) + 1.5 * stats$IQR(x)
}

#' prism_plot
#' @export
prism_plot <- function(data, tables, trt_sel,
                       time_sel, endpoint, format, cfb, power,
                       box_width = 2, axis_title_size = 30, axis_text_size = 24,
                       top_height = 2, bottom_height = 3, num_groups, type = "box",
                       inputs = NULL, same_ylim = FALSE) {
  {
    box::use(ggprism)
    box::use(ggplot2)
    box::use(dplyr)
    box::use(. / final_output)
    box::use(stats)
    box::use(. / prism_plot)
    box::use(tidyr)
    box::use(gridExtra)
    library(grid)
    library(gridExtra)
    library(lattice)
    library(ggprism)
  }
  order_groups <- match(
    c(
      "Wild Type", "Negative Control", "Other Comparator", "Positive Control", "Vehicle",
      grep(pattern = "Dose", x = levels(data$TreatmentNew), value = T)
    ),
    levels(data$TreatmentNew)
  )
  orig_groups <- dplyr$distinct(data, Treatment, TreatmentNew)
  orig_groups <- dplyr$mutate(orig_groups[order_groups, ], Treatment = as.character(Treatment))
  orig_groups <- dplyr$select(orig_groups, Treatment)
  orig_groups <- unlist(orig_groups)
  colors <- c(
    ggprism::ggprism_data$colour_palettes[[inputs$palette]],
    ggprism::ggprism_data$colour_palettes$pastel
  )[1:length(orig_groups)]

  data$Treatment <- factor(data$Treatment, levels = orig_groups)
  y_axis <- inputs$y_axisPrism
  tab1 <- tables$tab1
  tab2 <- tables$tab2
  tab3 <- tables$tab3

  trans_name <- final_output$transform_table()


  trans_name <- trans_name$transform_name[which(trans_name$power == power)]
  trans_name <- gsub("A ", "", trans_name)
  trans_name <- gsub("An ", "", trans_name)

  if (power == 1 | y_axis == "no_transform") {
    ylabel <- endpoint
    data <- dplyr$mutate(data, Response_Transformed = Response)
    ylabel <- endpoint
  }

  if (y_axis == "transform" & power != 1) {
    ylabel <- paste(trans_name, endpoint)
    var <- "Response_Transformed"
  }


  if (y_axis == "change_from_baseline" & power == 1) {
    data <- dplyr$mutate(data, Response_Transformed = Response_Transformed_bc)
    ylabel <- paste0("Change from Baseline \n", endpoint)
  }

  if (y_axis == "change_from_baseline" & power != 1) {
    data <- dplyr$mutate(data, Response_Transformed = Response_Transformed_bc)
    ylabel <- paste0("Change from Baseline \n", trans_name, endpoint)
  }

  p_vals <- dplyr$bind_rows(tab1, tab2, tab3)
  p_vals <- dplyr$select(p_vals, Treatment, `Time Points`, grep("p value from", colnames(p_vals)))
  p_vals <- dplyr$mutate_at(p_vals, .vars = 3:ncol(p_vals), .funs = ~ as.character(.))
  p_vals <- tidyr$pivot_longer(p_vals,
    cols = 3:ncol(p_vals),
    names_to = "group2",
    values_to = "p value"
  )

  p_vals <- dplyr$rename(p_vals, group1 = Treatment)
  p_vals <- dplyr$filter(
    p_vals,
    stats$complete.cases(p_vals),
    `Time Points` != "Average Over Time",
    `p value` != ""
  )

  p_vals <-
    dplyr$mutate(
      p_vals,
      group2 = gsub("p value from ", "", group2),
      `p value` = dplyr$if_else(`p value` == "< 0.001", "0.001", `p value`),
      `p value` = as.numeric(`p value`),
      sig = dplyr$case_when(
        `p value` > 0.05 ~ "ns",
        `p value` <= 0.05 & `p value` > 0.01 ~ "*",
        `p value` <= 0.01 &
          `p value` > 0.001 ~ "**",
        `p value` <= 0.001 &
          `p value` > 0.0001 ~ "***",
        `p value` < 0.001 ~ "****"
      )
    )
  p_vals <- dplyr$filter(p_vals, `p value` < 0.05)
  p_vals <- dplyr$filter(p_vals, `Time Points` == time_sel)
  p_vals <- dplyr$arrange(p_vals, group2, group1)
  p_vals <- dplyr$mutate(p_vals, y.position = seq(
    1.55 * max(data$Response_Transformed),
    2.5 * max(data$Response_Transformed),
    length.out = nrow(p_vals)
  ))
  p_vals <- dplyr$filter(
    p_vals,
    group1 %in% trt_sel,
    group2 %in% trt_sel
  )
  p_vals <- dplyr$mutate(p_vals, new_y.position = y.position + 1 * dplyr$row_number())



  data <-
    dplyr$filter(
      data,
      Time == time_sel,
      Treatment %in% trt_sel
    )
  data <- dplyr$group_by(data, Treatment)
  data <- dplyr$mutate(data, outlier = prism_plot$is.outlier(Response_Transformed))
  data <- dplyr$ungroup(data)

  data_max <- dplyr$group_by(data, Treatment)
  data_max <- dplyr$summarize(data_max,
    max = max(Response_Transformed),
    Mean_Response = mean(Response_Transformed),
    sd_Response = stats$sd(Response_Transformed)
  )
  data_max <- dplyr$rename(data_max, Response_Transformed = Mean_Response)
  data_max <-
    dplyr$mutate(data_max,
      error = dplyr$if_else(Response_Transformed < 0, Response_Transformed - sd_Response, Response_Transformed + sd_Response),
      ymin = dplyr$if_else(Response_Transformed < 0, error, Response_Transformed),
      ymax = dplyr$if_else(Response_Transformed < 0, Response_Transformed, error)
    )

  if (type == "box") {
    full_prism <- ggplot2$ggplot(
      dplyr$rename(data, group1 = Treatment),
      ggplot2$aes(x = group1, y = Response_Transformed, color = group1)
    ) +
      ggplot2$stat_boxplot(
        geom = "errorbar",
        width = 3 * length(unique(data$Treatment)) / num_groups^2,
        lwd = 1
      ) +
      ggplot2$geom_boxplot(
        ggplot2$aes(fill = group1),
        outlier.color = NA,
        lwd = ifelse(format == "word", 1, 2),
        fatten = 1,
        width = 3 * length(unique(data$Treatment)) / num_groups^2,
        alpha = 0.5
      ) +
      ggplot2$geom_jitter(
        ggplot2$aes(shape = group1),
        size = ifelse(format == "word", 1.5, 3),
        position = ggplot2$position_jitter(width = 0.2)
      ) +
      ggplot2$stat_summary(
        fun = "mean",
        color = "black",
        size = ifelse(format == "word", 0.2, 0.5),
        show.legend = FALSE
      ) +
      ggplot2$scale_y_continuous(limits = c(NA, 3 * max(data$Response_Transformed))) +
      ggplot2$scale_fill_manual(values = colors, breaks = orig_groups) +
      ggplot2$scale_color_manual(values = colors, breaks = orig_groups) +
      ggplot2$guides(y = "prism_offset_minor") +
      ggprism$theme_prism(base_size = ifelse(format == "word", 16, inputs$fontSize)) +
      ggplot2$theme(legend.position = "none") +
      ggplot2$ylab(ylabel) +
      ggplot2$xlab("Treatment")

    if (same_ylim) {
      ylim <- c(1.1 * min(0, min(data$Response_Transformed)), 1.1 * max(data$Response_Transformed))
    } else {
      ylim <- c(ggplot2$layer_scales(full_prism)$y$range$range[1], 1.1 * max(data$Response_Transformed))
    }

    bottom <- full_prism +
      ggplot2$scale_y_continuous(
        limits = ylim
      ) +
      ggplot2$theme(
        plot.margin = ggplot2$margin(
          t = -10,
          r = 0,
          b = 0,
          l = 0
        ),
        axis.title.y = ggplot2$element_text(margin = ggplot2$margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = ggplot2$element_text(margin = ggplot2$margin(t = 10, r = 0, b = 0, l = 0))
      )
  } else {
    full_prism <- ggplot2$ggplot(
      dplyr$rename(data_max, group1 = Treatment),
      ggplot2$aes(x = group1, y = Response_Transformed, color = group1)
    ) +
      ggplot2$geom_bar(
        ggplot2$aes(fill = group1),
        lwd = ifelse(format == "word", 1, 2),
        stat = "identity",
        width = 3 * length(unique(data$Treatment)) / num_groups^2,
        position = ggplot2$position_dodge(width = 0.7),
        alpha = 0.5
      ) +
      ggplot2$geom_jitter(
        data = dplyr$rename(data, group1 = Treatment),
        ggplot2$aes(y = Response_Transformed, shape = group1),
        size = ifelse(format == "word", 1.5, 3),
        position = ggplot2$position_jitter(width = 0.2)
      ) +
      ggplot2$geom_errorbar(
        ggplot2$aes(ymin = ymin, ymax = ymax),
        position = ggplot2$position_dodge(width = 0.7), width = 3 * length(unique(data$Treatment)) / num_groups^2,
        size = ifelse(format == "word", 1, 2)
      ) +
      ggplot2$guides(y = "prism_offset_minor") +
      ggplot2$scale_y_continuous(limits = c(NA, 1.1 * max(p_vals$new_y.position))) +
      ggplot2$scale_fill_manual(values = colors, breaks = orig_groups) +
      ggplot2$scale_color_manual(values = colors, breaks = orig_groups) +
      ggprism$theme_prism(base_size = ifelse(format == "word", 16, inputs$fontSize)) +
      ggplot2$theme(legend.position = "none") +
      ggplot2$ylab(ylabel) +
      ggplot2$xlab("Treatment")

    bottom <- full_prism +
      ggplot2$theme(
        plot.margin = ggplot2$margin(
          t = -10,
          r = 0,
          b = 0,
          l = 0
        ),
        axis.title.y = ggplot2$element_text(margin = ggplot2$margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = ggplot2$element_text(margin = ggplot2$margin(t = 10, r = 0, b = 0, l = 0))
      ) +
      ggplot2$scale_y_continuous(
        limits = c(1.1 * min(0, min(data$Response_Transformed)), 1.1 * max(data$Response_Transformed)),
        expand = ggplot2$expansion(mult = c(0, 0))
      )
  }
  if (format == "word") {
    bottom <- bottom + ggplot2$theme(
      axis.text = ggplot2$element_text(size = 8),
      axis.title = ggplot2$element_text(size = 10)
    )
  }
  if (nrow(p_vals) > 0 & ((as.logical(cfb) == TRUE & y_axis == "change_from_baseline") |
    (as.logical(cfb) != TRUE & y_axis != "change_from_baseline"))) {
    full_prism <- full_prism + ggprism$add_pvalue(
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
      ggplot2$scale_y_continuous(
        limits = c(0.9 * min(p_vals$new_y.position), NA),
        expand = ggplot2$expansion(mult = c(0, 0.1))
      )

    if (format == "word") {
      top <- top +
        ggplot2$theme(
          line = ggplot2$element_blank(),
          axis.title = ggplot2$element_blank(),
          axis.text = ggplot2$element_blank()
        ) +
        ggplot2$theme(plot.margin = ggplot2$margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0
        ))
    } else {
      top <- full_prism +
        ggplot2$scale_y_continuous(
          limits = c(0.9 * min(p_vals$new_y.position), NA),
          expand = ggplot2$expansion(mult = c(0.25, 0.1))
        ) +
        ggplot2$theme(
          line = ggplot2$element_blank(),
          axis.title = ggplot2$element_blank(),
          axis.text = ggplot2$element_blank()
        ) +
        ggplot2$theme(plot.margin = ggplot2$margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0
        ))
    }
    layout <- matrix(c(rep(1, 100 - inputs$bottom_percent), rep(2, inputs$bottom_percent)), ncol = 1)
    # if (nrow(p_vals) > 6 & nrow(p_vals) < 12) {
    #  layout <- rbind(1, 2)
    # }
    # if (nrow(p_vals) >= 12) {
    #  layout <- rbind(1, 1, 2)
    # }

    if (type == "box") {
      top <- top + ggplot2$ggtitle(paste("Box plot for Treatment Groups at", time_sel))
      combined <- gridExtra$grid.arrange(grobs = list(top, bottom), layout_matrix = layout)
      # ncol = 1,
      # heights=unit(c(100-bottom_percent,bottom_percent)/inputs$plotHeight, c('in', 'in')))
    } else {
      top <- top + ggplot2$ggtitle(paste("Bar Chart for Treatment Groups at", time_sel))
      combined <- gridExtra$grid.arrange(grobs = list(top, bottom), layout_matrix = layout)
      # ncol = 1,
      # heights=unit(c(100-bottom_percent,bottom_percent)/inputs$plotHeight,c('in', 'in')))
    }

    return(combined)
  } else {
    if (type == "box") {
      bottom <- bottom +
        ggplot2$ggtitle(paste("Box Plot for Treatment Groups at", time_sel)) +
        ggplot2$theme(
          plot.margin = ggplot2$margin(
            t = 0,
            r = 0,
            b = 0,
            l = 0
          ),
          axis.title.y = ggplot2$element_text(margin = ggplot2$margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = ggplot2$element_text(margin = ggplot2$margin(t = 10, r = 0, b = 0, l = 0))
        )
    } else {
      bottom <- bottom +
        ggplot2$ggtitle(paste("Bar Chart for Treatment Groups at", time_sel)) +
        ggplot2$theme(
          plot.margin = ggplot2$margin(
            t = 0,
            r = 0,
            b = 0,
            l = 0
          ),
          axis.title.y = ggplot2$element_text(margin = ggplot2$margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = ggplot2$element_text(margin = ggplot2$margin(t = 10, r = 0, b = 0, l = 0))
        )
      return(bottom)
    }
  }

  TRUE
}
