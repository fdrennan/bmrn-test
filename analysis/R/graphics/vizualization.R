#'
#' @export
test_plot_theme <- function() {
  {
    box::use(ggplot2)
  }
  font_size <- 10
  list(
    ggplot2$theme_bw(),
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(angle = 45, vjust = 0.75, hjust = 0.75, size = 8),
      panel.grid.major = ggplot2$element_blank(),
      panel.grid.minor = ggplot2$element_blank(),
      axis.text = ggplot2$element_text(size = font_size, face = "bold"),
      axis.title = ggplot2$element_text(size = font_size),
      strip.text = ggplot2$element_text(size = font_size),
      plot.title = ggplot2$element_text(size = font_size),
      axis.title.y = ggplot2$element_text(margin = ggplot2$margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = ggplot2$element_text(margin = ggplot2$margin(t = 10, r = 0, b = 0, l = 0))
    )
  )
}

#' ylab_move
#' @export
ylab_move <- function(plot, x_parameter, y_parameter) {
  {
    box::use(utils)
  }
  utils$str(plot[["x"]][["layout"]][["annotations"]])
  plot[["x"]][["layout"]][["annotations"]][[1]][["y"]] <- -x_parameter
  plot[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -y_parameter
  return(plot)
}

#' bold_interactive
#' @export
bold_interactive <- function(plot_orig, panel) {
  {
    box::use(plotly)
  }
  plot <- plot_orig
  if (any(class(plot) == "plotly") == FALSE) {
    plot <- plotly$ggplotly(plot)
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
#' @export
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
#' @export
vizualization <- function(transformed_data, power = 1, endpoint, ui_sel, palette = "floral") {
  {
    box::use(dplyr)
    box::use(ggprism)
    box::use(tidyr)
    box::use(ggplot2)
    box::use(. / vizualization)
    box::use(stats)
  }

  order_groups <- match(
    c(
      "Wild Type", "Negative Control", "Other Comparator", "Positive Control", "Vehicle",
      grep(pattern = "Dose", x = levels(transformed_data$TreatmentNew), value = T)
    ),
    levels(transformed_data$TreatmentNew)
  )

  orig_groups <- dplyr$distinct(transformed_data, Treatment, TreatmentNew)
  orig_groups <- dplyr$mutate(orig_groups[order_groups, ], Treatment = as.character(Treatment))
  orig_groups <- dplyr$select(orig_groups, Treatment)
  orig_groups <- unlist(orig_groups)


  colors <- c(
    ggprism::ggprism_data$colour_palettes[[palette]],
    ggprism::ggprism_data$colour_palettes$pastel
  )[1:length(orig_groups)]
  linetype <- (1:length(orig_groups) %% 6) + 1

  # colors = viridis(length(orig_groups))
  transformed_data <- dplyr$filter(transformed_data, Treatment %in% ui_sel$trt_sel)

  # keep levels of transformed data time, when input updates
  input_time <- ui_sel$time_sel
  original_time <- levels(transformed_data$Time)

  input_time <- original_time[original_time %in% input_time]

  transformed_data <- dplyr$filter(transformed_data, Time %in% input_time)

  transform_table <- data.frame(
    power = c(2, 1, 0.5, 0, -0.5, -1),
    transform_name = c(
      "Squared", "Identity",
      "Square Root", "Log",
      "Inverse Square Root",
      "Inverse"
    )
  )

  if ((ui_sel$y_axis == "transform" & power == 1) | ui_sel$y_axis == "no_transform") {
    transformed_data <- dplyr$select(transformed_data, -c(Response_Transformed, Baseline_Transformed))
    transformed_data <-
      dplyr$rename(transformed_data,
        Baseline_Transformed = Baseline,
        Response_Transformed = Response
      )
    ylabel <- endpoint
  }

  if (ui_sel$y_axis == "transform" & power != 1) {
    ylabel <- paste(
      transform_table$transform_name[power == transform_table$power],
      "\n Transformed", endpoint
    )
  }

  if (ui_sel$y_axis != "change_from_baseline" && any(ui_sel$time_sel %in% "Baseline")) {
    times <- setdiff(input_time, "Baseline")
    transformed_data <-
      dplyr$mutate(transformed_data,
        Baseline_Transformed = as.numeric(Baseline_Transformed),
        Response_Transformed = as.numeric(Response_Transformed)
      )
    transformed_data <-
      tidyr$pivot_wider(transformed_data, names_from = "Time", values_from = "Response_Transformed")
    transformed_data <- tidyr$pivot_longer(
      transformed_data,
      cols = c("Baseline_Transformed", times), values_to = "Response_Transformed",
      names_to = "Time"
    )
    transformed_data <-
      dplyr$mutate(
        transformed_data,
        Time = as.character(Time),
        Time = dplyr$if_else(Time == "Baseline_Transformed", "Baseline", Time),
        Time = factor(Time, levels = c("Baseline", times))
      )
    transformed_data <-
      dplyr$filter(
        transformed_data,
        !is.na(Response_Transformed)
      )
    transformed_data <- dplyr$group_by(transformed_data, SubjectID, Treatment, TreatmentNew, Time)
    transformed_data <- dplyr$summarize(transformed_data, Response_Transformed = mean(Response_Transformed))
    transformed_data <- dplyr$ungroup(transformed_data)
  }

  if (ui_sel$y_axis == "change_from_baseline" & power == 1) {
    transformed_data <- dplyr$mutate(transformed_data, Response_Transformed = Response_Transformed_bc)
    ylabel <- paste("Change from Baseline\n", endpoint)
  }

  if (ui_sel$y_axis == "change_from_baseline" & power != 1) {
    transformed_data <-
      dplyr$mutate(transformed_data, Response_Transformed = Response_Transformed_bc)
    ylabel <- paste(
      "Change from Baseline\n", transform_table$transform_name[power == transform_table$power],
      endpoint
    )
  }

  transformed_data$Treatment <- factor(transformed_data$Treatment, levels = orig_groups)
  transformed_data_sum <-
    dplyr$group_by(transformed_data, Treatment, TreatmentNew, Time)
  transformed_data_sum <- dplyr$summarize(transformed_data_sum,
    Mean_Response = mean(Response_Transformed),
    sd_Response = stats$sd(Response_Transformed)
  )
  transformed_data_sum <- dplyr$mutate(transformed_data_sum,
    error = dplyr$if_else(Mean_Response < 0, Mean_Response - sd_Response, Mean_Response + sd_Response),
    ymin = dplyr$if_else(Mean_Response < 0, error, Mean_Response),
    ymax = dplyr$if_else(Mean_Response < 0, Mean_Response, error)
  )


  bar_plot_orig_scale <- ggplot2$ggplot(
    data = transformed_data_sum,
    ggplot2$aes(x = Time, y = Mean_Response)
  ) +
    ggplot2$scale_x_discrete() +
    ggplot2$scale_y_continuous(
      limits = c(1.5 * min(0, min(transformed_data_sum$ymin)), 1.5 * max(transformed_data_sum$ymax)),
      expand = ggplot2$expansion(mult = c(0, 0))
    ) +
    ggplot2$geom_bar(
      stat = "identity",
      ggplot2$aes(color = Treatment), fill = "white",
      width = 0.5, position = ggplot2$position_dodge(width = 0.7)
    ) +
    ggplot2$geom_errorbar(ggplot2$aes(
      ymin = ymin, ymax = ymax,
      color = Treatment
    ), position = ggplot2$position_dodge(width = 0.7), size = 0.75) +
    ggplot2$geom_point(
      ggplot2$aes(y = Response_Transformed, color = Treatment),
      show.legend = FALSE,
      data = transformed_data, size = 0.7, position = ggplot2$position_dodge(width = 0.7)
    ) +
    ggplot2$labs(color = "Treatment") +
    ggplot2$ylab(ylabel) +
    ggplot2$ggtitle("Bar Chart for Each Group Over Time") +
    vizualization$test_plot_theme() +
    ggplot2$scale_color_manual(values = colors, breaks = orig_groups)


  box_plot_transformed <- ggplot2$ggplot(data = transformed_data, ggplot2$aes(x = Time, y = Response_Transformed, label = SubjectID)) +
    ggplot2$geom_boxplot(ggplot2$aes(color = Treatment), show.legend = FALSE) +
    ggplot2$geom_point(
      position = ggplot2$position_dodge(width = 0.1), ggplot2$aes(color = Treatment),
      show.legend = FALSE, size = 0.7
    ) +
    ggplot2$labs(color = "Treatment") +
    ggplot2$facet_wrap(Treatment ~ ., nrow = ui_sel$num_rows) +
    ggplot2$ylab(ylabel) +
    ggplot2$stat_summary(fun = "mean", color = "black", show.legend = FALSE, size = 0.2) +
    ggplot2$ggtitle("Box Plot for Each Group Over Time") +
    vizualization$test_plot_theme() +
    ggplot2$scale_color_manual(values = colors, breaks = orig_groups)


  sub_line_plot <- ggplot2$ggplot(
    data = transformed_data,
    ggplot2$aes(x = Time, y = Response_Transformed, group = SubjectID)
  ) +
    ggplot2$geom_line(ggplot2$aes(color = Treatment), size = 0.2, show.legend = FALSE) +
    ggplot2$geom_point(ggplot2$aes(color = Treatment), show.legend = FALSE) +
    ggplot2$ylab(ylabel) +
    ggplot2$facet_wrap(Treatment ~ ., nrow = ui_sel$num_rows) +
    ggplot2$ggtitle("Trajectory of Each Subject by Group") +
    vizualization$test_plot_theme() +
    ggplot2$scale_color_manual(values = colors, breaks = orig_groups)

  line_plot <- ggplot2$ggplot(transformed_data_sum, ggplot2$aes(
    x = Time, y = Mean_Response,
    color = Treatment, group = Treatment
  )) +
    ggplot2$geom_point(size = 1.25, show.legend = F) +
    ggplot2$geom_line(
      ggplot2$aes(
        x = Time, y = Mean_Response, linetype = Treatment, color = Treatment
      ),
      size = 1.25,
      show.legend = T
    ) +
    ggplot2$geom_errorbar(
      ggplot2$aes(
        ymin = Mean_Response - sd_Response, ymax = Mean_Response + sd_Response,
        color = Treatment
      ),
      width = .5, show.legend = F
    ) +
    ggplot2$theme(legend.position = "bottom") +
    ggplot2$labs(color = "Treatment") +
    ggplot2$ylab(ylabel) +
    ggplot2$ggtitle("Mean and Standard Error Bars for Each Group Over Time") +
    vizualization$test_plot_theme() +
    ggplot2$scale_color_manual(values = colors, breaks = orig_groups) +
    ggplot2$scale_linetype_manual(values = linetype, breaks = orig_groups)

  # Has not been implemented yet
  return(list(
    box = box_plot_transformed,
    group_line = line_plot,
    bar = bar_plot_orig_scale,
    sub_line = sub_line_plot
  ))
}
