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
                       top_height = 1, bottom_height = 4, num_groups, type = "box",
                       inputs = NULL) {
  y_axis = inputs$y_axisPrism
  tab1 <- tables[[2]]
  tab2 <- tables[[3]]
  tab3 <- tables[[4]]
  
  trans_name <- transform_table()
  
  
  trans_name <- trans_name$transform_name[which(trans_name$power == power)]
  trans_name <- gsub("A ", "", trans_name)
  trans_name <- gsub("An ", "", trans_name)
  
  if (power == 1 | y_axis == 'no_transform') {
    ylab <- endpoint
    data <- data %>%
      mutate(Response_Transformed = Response)
  }
  
  if (y_axis == 'transform') {
    ylab <- paste(trans_name, endpoint)
    var = 'Response_Transformed'
  }
  
  
  if (y_axis == 'change_from_baseline' & power == 1) {
    data <- data %>%
      mutate(Response_Transformed = Response_Transformed_bc)
    ylab <- paste0("Change from Baseline \n", endpoint)
  }
  
  if (y_axis == 'change_from_baseline' & power != 1) {
    data <- data %>%
      mutate(Response_Transformed = Response_Transformed_bc)
    ylab <- paste0(trans_name, "\n Change from Baseline ", endpoint)
  }
  
  p_vals <- bind_rows(tab1, tab2, tab3) %>%
    select(Treatment, `Times Included`, grep("p value from", colnames(.))) %>%
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
      `Times Included` != "Average Over Time",
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
    filter(`Times Included` == time_sel) %>%
    arrange(group2, group1) %>%
    mutate(y.position = seq(
      1.25 * max(data$Response_Transformed),
      2.2 * max(data$Response_Transformed),
      length.out = nrow(.)
    )) %>%
    filter(
      group1 %in% trt_sel,
      group2 %in% trt_sel
    )
  
  
  
  correct_level_order <- data %>%
    arrange(TreatmentNew) %>%
    distinct(Treatment, TreatmentNew) %>%
    dplyr::select(Treatment) %>%
    unlist()
  
  data <- data %>%
    filter(Time == time_sel) %>%
    group_by(Treatment) %>%
    mutate(
      outlier = is.outlier(Response_Transformed),
      Treatment = factor(Treatment, levels = correct_level_order)
    ) %>%
    ungroup()
  
  if (type == "box") {
    full_prism <- ggplot(
      data %>% rename(group1 = Treatment),
      aes(x = group1, y = Response_Transformed, color = group1)
    ) +
      stat_boxplot(
        geom = "errorbar",
        width = length(unique(data$Treatment)) / num_groups^2,
        lwd = 1
      ) +
      geom_boxplot(
        aes(fill = group1),
        outlier.color = NA,
        lwd = 2,
        fatten = 1,
        width = length(unique(data$Treatment)) / num_groups^2
      ) +
      geom_jitter(
        # data = data %>% filter(outlier) %>% rename(group1 = Treatment),
        # plot outliers only
        # aes(shape = group1),
        size = 3,
        position = position_dodge(width = 0.2)
      ) +
      stat_summary(
        fun = "mean",
        color = "black",
        show.legend = FALSE
      ) +
      scale_y_continuous(limits = c(NA, 3 * max(data$Response_Transformed))) +
      # scale_shape_manual(values = 15:20) +
      scale_color_prism("floral") +
      scale_fill_prism("floral") +
      guides(y = "prism_offset_minor") +
      theme_prism(base_size = inputs$fontSize, palette = inputs$palette, border = inputs$border) +
      theme(legend.position = "none") +
      ylab(ylab) +
      xlab("Treatment")
  } else {
    data_max <- data %>%
      group_by(Treatment) %>%
      summarize(
        max = max(Response_Transformed),
        Mean_Response = mean(Response_Transformed),
        sd_Response = sd(Response_Transformed)
      ) %>%
      rename(Response_Transformed = Mean_Response)
    
    full_prism <- ggplot(
      data_max %>% rename(group1 = Treatment),
      aes(x = group1, y = Response_Transformed, color = group1)
    ) +
      geom_bar(
        fill = "white",
        lwd = 2,
        stat = "identity",
        width = 0.5,
        position = position_dodge(width = 0.7)
      ) +
      geom_jitter(
        data = data %>% rename(group1 = Treatment),
        aes(y = Response_Transformed),
        size = 3,
        position = position_dodge(width = 0.2)
      ) +
      stat_summary(
        data = data %>% rename(group1 = Treatment),
        fun = "mean",
        color = "black",
        show.legend = FALSE
      ) +
      geom_errorbar(
        aes(ymin = Response_Transformed, ymax = Response_Transformed + sd_Response),
        position = position_dodge(width = 0.7), width = 0.35,
        size = 2
      ) +
      scale_y_continuous(limits = c(NA, 3 * max(data_max$Response_Transformed))) +
      # scale_shape_manual(values = 15:20) +
      scale_color_prism("floral") +
      scale_fill_prism("floral") +
      guides(y = "prism_offset_minor") +
      theme_prism(base_size = 16) +
      theme(legend.position = "none") +
      ylab(ylab) +
      xlab("Treatment")
  }
  
  bottom <- full_prism +
    scale_y_continuous(limits = c(NA, 1.1 * max(data$Response_Transformed))) +
    theme(plot.margin = margin(
      t = -10,
      r = 0,
      b = 0,
      l = 0
    ))
  if (format == "word") {
    bottom <- bottom + theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10)
    )
  }
  
  if (nrow(p_vals) > 0 & ((as.logical(cfb) == TRUE & y_axis == 'change_from_baseline')|
                          (as.logical(cfb) != TRUE & y_axis != 'change_from_baseline'))) {
    full_prism <- full_prism + add_pvalue(
      p_vals,
      label = "{sig}",
      tip.length = 0.02,
      label.size = 10,
      color = "black",
      size = 2
    )
    
    
    top <- full_prism +
      scale_y_continuous(limits = c(
        1.1 * max(data$Response_Transformed),
        1.1 * max(p_vals$y.position)
      )) +
      theme(
        line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()
      ) +
      theme(plot.margin = margin(
        t = 0,
        r = 0,
        b = -10,
        l = 0
      ))
    return(ggarrange(
      plotlist = list(top, bottom),
      ncol = 1,
      heights = c((1 - 1 / nrow(p_vals)) * top_height, bottom_height)
    ))
  } else {
    return(bottom)
  }
}
