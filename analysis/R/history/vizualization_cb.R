#'
#' @export
vizualization_cb <- function(transformed_data, power, endpoint, transformation) {
  {
    box::use(stats)
  }
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
      dplyr$select(-c(Response_Transformed, Baseline_Transformed)) %>%
      dplyr$rename(
        Baseline_Transformed = Baseline,
        Response_Transformed = Response
      )
    ylabel <- paste("Change from Baseline\n", endpoint)
  } else {
    ylabel <- paste(
      transform_table$transform_name[power == transform_table$power],
      "\n Transformed Change from Baseline\n", endpoint
    )
  }

  transformed_data_sum <- transformed_data %>%
    dplyr$group_by(Treatment, Time) %>%
    dplyr$summarize(
      Mean_Response = mean(Response_Transformed_bc),
      sd_Response = stats$sd(Response_Transformed_bc)
    )

  bar_plot_orig_scale <- ggplot2$ggplot(data = transformed_data_sum, ggplot2$aes(x = Time, y = Mean_Response)) +
    ggplot2$geom_bar(
      stat = "identity", position = "dodge",
      ggplot2$aes(color = Treatment), fill = "white"
    ) +
    ggplot2$geom_errorbar(ggplot2$aes(
      ymin = Mean_Response - sd_Response, ymax = Mean_Response + sd_Response,
      color = Treatment
    ), position = "dodge", size = 0.75) +
    ggplot2$geom_point(
      position = position_jitterdodge(dodge.width = 0.85),
      ggplot2$aes(y = Response_Transformed_bc, color = Treatment), show.legend = FALSE,
      data = transformed_data
    ) +
    theme_bw() +
    ggplot2$labs(color = "Treatment") +
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(angle = 45, vjust = 0.75, hjust = 0.75),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = ggplot2$element_text(size = 14),
      axis.title = ggplot2$element_text(size = 14),
      strip.text = ggplot2$element_text(size = 14),
      title = ggplot2$element_text(size = 16)
    ) +
    ggplot2$ylab(ylabel) +
    ggplot2$ggtitle("Bar Chart for Each Group Across Time Points")


  box_plot_transformed <- ggplot2$ggplot(data = transformed_data, ggplot2$aes(x = Time, y = Response_Transformed_bc)) +
    ggplot2$geom_boxplot(ggplot2$aes(color = Treatment), show.legend = FALSE) +
    ggplot2$geom_jitter(width = 0.1, ggplot2$aes(color = Treatment), show.legend = FALSE, size = 0.3) +
    theme_bw() +
    ggplot2$labs(color = "Treatment") +
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(angle = 45, vjust = 0.75, hjust = 0.75),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = ggplot2$element_text(size = 14),
      axis.title = ggplot2$element_text(size = 14),
      strip.text = ggplot2$element_text(size = 14),
      title = ggplot2$element_text(size = 16)
    ) +
    ggplot2$facet_wrap(Treatment ~ ., nrow = 1) +
    ggplot2$ylab(ylabel) +
    ggplot2$stat_summary(fun = "mean", color = "black", show.legend = FALSE, size = 0.2) +
    ggplot2$ggtitle("Box Plot for Each Group Across Time Points")


  sub_line_plot <- ggplot2$ggplot(
    data = transformed_data,
    ggplot2$aes(x = Time, y = Response_Transformed_bc, group = SubjectID)
  ) +
    ggplot2$geom_line(ggplot2$aes(color = Treatment), size = 1, show.legend = FALSE) +
    theme_bw() +
    ggplot2$theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = ggplot2$element_text(
        angle = 45, vjust = 0.75, hjust = 0.75,
        size = 14
      ),
      axis.text.y = ggplot2$element_text(size = 14),
      axis.title = ggplot2$element_text(size = 14),
      strip.text = ggplot2$element_text(size = 14),
      title = ggplot2$element_text(size = 16)
    ) +
    ggplot2$ylab(ylabel) +
    ggplot2$facet_wrap(Treatment ~ ., nrow = 1) +
    ggplot2$ggtitle("Trajectory of Each Subject by Group")

  # line_plot <-
  #   ggplot2$ggplot(data = transformed_data_sum,
  #          ggplot2$aes(x = Time, y = Response, color = Treatment)) +
  #   ggplot2$geom_point() +
  #   geom_smooth(method = "lm", alpha = .15, ggplot2$aes(fill = Treatment))
  #   #geom_line(ggplot2$aes(color = Treatment, linetype = Treatment), size = 1.5) +
  #   # ggplot2$facet_wrap(scale ~ ., scales = "free_y") +
  #   theme_bw() +
  #   ggplot2$theme(legend.position = "bottom") +
  #   ggplot2$labs(color = "Treatment", linetype = "Treatment") +
  #   ggplot2$theme(axis.text.x =ggplot2$element_text(angle = 45, vjust = 0.75, hjust = 0.75))

  # Cheng's suggestion 3/8/2022
  line_plot <- ggplot2$ggplot(transformed_data_sum, ggplot2$aes(
    x = Time, y = Mean_Response,
    color = Treatment, group = Treatment
  )) +
    ggplot2$geom_point(size = 1.25) +
    ggplot2$geom_line(
      ggplot2$aes(
        x = Time, y = Mean_Response,
        color = Treatment, linetype = Treatment
      ),
      size = 1.25,
      show.legend = F
    ) +
    ggplot2$geom_errorbar(ggplot2$aes(ymin = Mean_Response - sd_Response, ymax = Mean_Response + sd_Response),
      width = .5, show.legend = F
    ) +
    theme_bw() +
    ggplot2$theme(legend.position = "bottom") +
    ggplot2$labs(color = "Treatment", linetype = "Treatment") +
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(angle = 45, vjust = 0.75, hjust = 0.75),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = ggplot2$element_text(size = 14),
      axis.title = ggplot2$element_text(size = 14),
      strip.text = ggplot2$element_text(size = 14),
      title = ggplot2$element_text(size = 16),
      legend.text = ggplot2$element_text(size = 14),
      legend.title = ggplot2$element_text(size = 16)
    ) +
    ggplot2$ylab(ylabel) +
    ggplot2$ggtitle("Line Plot for Each Group Across Time Points") +
    ggplot2$guides(colour = guide_legend(override.aes = list(size = 10)))


  # ggplotly(line_plot) #Interactive plots
  # ggplotly(box_plot) #Interactive plots
  # #Combine plots into one

  # combined_plot <- ggarrange(
  #   plotlist = list(box_plot_transformed,
  #                   line_plot,
  #                   bar_plot_orig_scale,
  #                   sub_line_plot),
  #   common.legend = TRUE, legend = "none")
  # #, labels = 'AUTO')

  # Has not been implemented yet
  return(return(list(
    box = box_plot_transformed,
    group_line = line_plot,
    bar = bar_plot_orig_scale,
    sub_line = sub_line_plot
  )))
}
