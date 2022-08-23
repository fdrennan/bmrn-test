#' test_plot_theme
#' @export test_plot_theme
test_plot_theme <- function() {
  font_size <- 10
  list(
    theme_bw(),
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.75, size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = font_size, face = "bold"),
      axis.title = element_text(size = font_size),
      strip.text = element_text(size = font_size),
      plot.title = element_text(size = font_size),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
    )
  )
}

#' ylab_move
#' @export ylab_move

ylab_move <- function(plot, x_parameter, y_parameter) {
  str(plot[["x"]][["layout"]][["annotations"]])
  plot[["x"]][["layout"]][["annotations"]][[1]][["y"]] <- -x_parameter
  plot[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -y_parameter
  return(plot)
}

#' bold_interactive
#' @export bold_interactive

bold_interactive <- function(plot_orig, panel) {
  #
  plot <- plot_orig
  if (any(class(plot) == "plotly") == FALSE) {
    plot <- ggplotly(plot)
  }
  plot$x$layout$legend$title$text <- "<b>Treatment"
  plot$x$layout$title$ticktext <- paste("<b>", plot$x$layout$title$ticktext)
  plot$x$layout$title$text <- paste("<b>", plot$x$layout$title$text)
  if (panel) {
    for (i in grep("xaxis", names(plot$x$layout), value = TRUE)) {
      plot$x$layout[[i]]$ticktext <- paste("<b>", plot$x$layout[[i]]$ticktext)
    }
    for (i in 1:length(plot$x$layout$annotations)) {
      plot$x$layout$annotations[[i]]$text <- paste("<b>", plot$x$layout$annotations[[i]]$text)
    }
    plot$x$layout$yaxis$ticktext <- paste("<b>", plot$x$layout$yaxis$ticktext)
  } else {
    plot$x$layout$xaxis$title[1] <- "<b> Time"
    plot$x$layout$yaxis$ticktext <- paste("<b>", plot$x$layout$yaxis$ticktext)
    plot$x$layout$xaxis$ticktext <- paste("<b>", plot$x$layout$xaxis$ticktext)

    plot$x$layout$yaxis$title$text <- paste("<b>", plot$x$layout$yaxis$title$text)
  }
  return(plot)
}

#' label_fix
#' @export label_fix
label_fix <- function(plot) {
  for (i in seq_along(plot$x$data)) {
    # Is the layer the first entry of the group?
    is_first <- grepl("^\\(.*?,1\\)", plot$x$data[[i]]$name)
    # Extract the group identifier and assign it to the name and legendgroup arguments
    plot$x$data[[i]]$name <- gsub("^\\((.*?),\\d+\\)", "\\1", plot$x$data[[i]]$name)
    plot$x$data[[i]]$name <- gsub("^\\((.*?),\\d+,NA\\)", "\\1", plot$x$data[[i]]$name)
    plot$x$data[[i]]$name <- paste("<b>", plot$x$data[[i]]$name)
    plot$x$data[[i]]$legendgroup <- plot$x$data[[i]]$name
    # Show the legend only for the first layer of the group
    if (is_first) plot$x$data[[i]]$showlegend <- FALSE
  }
  return(plot)
}


#' vizualization
#' @export vizualization
vizualization <- function(transformed_data, power = 1, endpoint, baseline, transformation, ui_sel) {
  orig_groups <- levels(factor(transformed_data$Treatment))
  colors <- c(
    ggprism_data$colour_palettes$floral,
    ggprism_data$colour_palettes$pastel
  )[1:length(orig_groups)]
  linetype <- (1:length(orig_groups) %% 6) + 1

  # colors = viridis(length(orig_groups))
  transformed_data <- filter(transformed_data, Treatment %in% ui_sel$trt_sel)
  transformed_data <- filter(transformed_data, Time %in% ui_sel$time_sel)

  transform_table <- data.frame(
    power = c(2, 1, 0.5, 0, -0.5, -1),
    transform_name = c(
      "Squared", "Identity",
      "Square Root", "Log",
      "Inverse Square Root",
      "Inverse"
    )
  )

  if (!transformation | power == 1) {
    transformed_data <- transformed_data %>%
      select(-c(Response_Transformed, Baseline_Transformed)) %>%
      rename(
        Baseline_Transformed = Baseline,
        Response_Transformed = Response
      )
    ylabel <- paste(endpoint)
  } else {
    ylabel <- paste(
      transform_table$transform_name[power == transform_table$power],
      "\n Transformed", endpoint
    )
  }
  if (!baseline || ui_sel %in% "Baseline") {
    # Treatment
    times <- unique(as.character(transformed_data$Time))
    transformed_data <- transformed_data %>%
      mutate(
        Baseline_Transformed = as.numeric(Baseline_Transformed),
        Response_Transformed = as.numeric(Response_Transformed)
      ) %>%
      pivot_wider(names_from = "Time", values_from = "Response_Transformed") %>%
      pivot_longer(
        cols = c("Baseline_Transformed", times), values_to = "Response_Transformed",
        names_to = "Time"
      ) %>%
      mutate(
        Time = as.character(Time),
        Time = if_else(Time == "Baseline_Transformed", "Baseline", Time),
        Time = factor(Time, levels = c("Baseline", times))
      ) %>%
      filter(
        !is.na(Response_Transformed)
      ) %>%
      group_by(SubjectID, Treatment, TreatmentNew, Time) %>%
      summarize(Response_Transformed = mean(Response_Transformed)) %>%
      ungroup()
  }
  
  if(baseline){
    transformed_data = transformed_data %>%
      mutate(Response_Transformed = Response_Transformed_bc)
  }
  
  correct_level_order <- transformed_data %>%
    arrange(TreatmentNew) %>%
    distinct(Treatment, TreatmentNew) %>%
    dplyr::select(Treatment) %>%
    unlist()

  transformed_data <- transformed_data %>%
    mutate(Treatment = factor(Treatment, levels = correct_level_order))

  transformed_data_sum <- transformed_data %>%
    group_by(Treatment, TreatmentNew, Time) %>%
    summarize(
      Mean_Response = mean(Response_Transformed),
      sd_Response = sd(Response_Transformed)
    ) %>%
    mutate(
      error = if_else(Mean_Response < 0, Mean_Response - sd_Response, Mean_Response + sd_Response),
      ymin = if_else(Mean_Response < 0, error, Mean_Response),
      ymax = if_else(Mean_Response < 0, Mean_Response, error)
    )


  bar_plot_orig_scale <- ggplot(data = transformed_data_sum, aes(x = Time, y = Mean_Response)) +
    scale_x_discrete() +
    scale_y_continuous(
      limits = c(1.5 * min(0, min(transformed_data_sum$ymin)), 1.5 * max(transformed_data_sum$ymax)),
      expand = expansion(mult = c(0, 0))
    ) +
    geom_bar(
      stat = "identity",
      aes(color = Treatment), fill = "white",
      width = 0.5, position = position_dodge(width = 0.7)
    ) +
    geom_errorbar(aes(
      ymin = ymin, ymax = ymax,
      color = Treatment
    ), position = position_dodge(width = 0.7), size = 0.75) +
    geom_point(
      aes(y = Response_Transformed, color = Treatment),
      show.legend = FALSE,
      data = transformed_data, size = 0.7, position = position_dodge(width = 0.7)
    ) +
    labs(color = "Treatment") +
    ylab(ylabel) +
    ggtitle("Bar Chart for Each Group Over Time") +
    test_plot_theme() +
    scale_color_manual(values = colors, breaks = orig_groups)


  box_plot_transformed <- ggplot(data = transformed_data, aes(x = Time, y = Response_Transformed, label = SubjectID)) +
    geom_boxplot(aes(color = Treatment), show.legend = FALSE) +
    geom_point(position = position_dodge(width = 0.1), aes(color = Treatment), show.legend = FALSE, size = 0.7) +
    labs(color = "Treatment") +
    facet_wrap(Treatment ~ ., nrow = 1) +
    ylab(ylabel) +
    stat_summary(fun = "mean", color = "black", show.legend = FALSE, size = 0.2) +
    ggtitle("Box Plot for Each Group Over Time") +
    test_plot_theme() +
    scale_color_manual(values = colors, breaks = orig_groups)


  sub_line_plot <- ggplot(
    data = transformed_data,
    aes(x = Time, y = Response_Transformed, group = SubjectID)
  ) +
    geom_line(aes(color = Treatment), size = 0.2, show.legend = FALSE) +
    geom_point(aes(color = Treatment), show.legend = FALSE) +
    ylab(ylabel) +
    facet_wrap(Treatment ~ ., nrow = 1) +
    ggtitle("Trajectory of Each Subject by Group") +
    test_plot_theme() +
    scale_color_manual(values = colors, breaks = orig_groups)

  line_plot <- ggplot(transformed_data_sum, aes(
    x = Time, y = Mean_Response,
    color = Treatment, group = Treatment
  )) +
    geom_point(size = 1.25, show.legend = F) +
    geom_line(aes(
      x = Time, y = Mean_Response, linetype = Treatment, color = Treatment
    ),
    size = 1.25,
    show.legend = T
    ) +
    geom_errorbar(aes(
      ymin = Mean_Response - sd_Response, ymax = Mean_Response + sd_Response,
      color = Treatment
    ),
    width = .5, show.legend = F
    ) +
    theme(legend.position = "bottom") +
    labs(color = "Treatment") +
    ylab(ylabel) +
    ggtitle("Mean and Standard Error Bars for Each Group Over Time") +
    # guides(colour = guide_legend(override.aes = list(size = 10))) +
    test_plot_theme() +
    scale_color_manual(values = colors, breaks = orig_groups) +
    scale_linetype_manual(values = linetype, breaks = orig_groups)

  # Has not been implemented yet
  return(list(
    box = box_plot_transformed,
    group_line = line_plot,
    bar = bar_plot_orig_scale,
    sub_line = sub_line_plot
  ))
}
