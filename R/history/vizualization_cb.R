#' vizualization_cb
#' @export vizualization_cb

vizualization_cb <- function(transformed_data, power, endpoint, transformation) {
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
      rename(
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
    dplyr$summarize((
      Mean_Response = mean(Response_Transformed_bc),
      sd_Response = sd(Response_Transformed_bc)
    )

  bar_plot_orig_scale <- ggplot(data = transformed_data_sum, aes(x = Time, y = Mean_Response)) +
    geom_bar(
      stat = "identity", position = "dodge",
      aes(color = Treatment), fill = "white"
    ) +
    geom_errorbar(aes(
      ymin = Mean_Response - sd_Response, ymax = Mean_Response + sd_Response,
      color = Treatment
    ), position = "dodge", size = 0.75) +
    geom_point(
      position = position_jitterdodge(dodge.width = 0.85),
      aes(y = Response_Transformed_bc, color = Treatment), show.legend = FALSE,
      data = transformed_data
    ) +
    theme_bw() +
    labs(color = "Treatment") +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.75),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      strip.text = element_text(size = 14),
      title = element_text(size = 16)
    ) +
    ylab(ylabel) +
    ggtitle("Bar Chart for Each Group Across Time Points")


  box_plot_transformed <- ggplot(data = transformed_data, aes(x = Time, y = Response_Transformed_bc)) +
    geom_boxplot(aes(color = Treatment), show.legend = FALSE) +
    geom_jitter(width = 0.1, aes(color = Treatment), show.legend = FALSE, size = 0.3) +
    theme_bw() +
    labs(color = "Treatment") +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.75),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      strip.text = element_text(size = 14),
      title = element_text(size = 16)
    ) +
    facet_wrap(Treatment ~ ., nrow = 1) +
    ylab(ylabel) +
    stat_summary(fun = "mean", color = "black", show.legend = FALSE, size = 0.2) +
    ggtitle("Box Plot for Each Group Across Time Points")


  sub_line_plot <- ggplot(
    data = transformed_data,
    aes(x = Time, y = Response_Transformed_bc, group = SubjectID)
  ) +
    geom_line(aes(color = Treatment), size = 1, show.legend = FALSE) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(
        angle = 45, vjust = 0.75, hjust = 0.75,
        size = 14
      ),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 14),
      strip.text = element_text(size = 14),
      title = element_text(size = 16)
    ) +
    ylab(ylabel) +
    facet_wrap(Treatment ~ ., nrow = 1) +
    ggtitle("Trajectory of Each Subject by Group")

  # line_plot <-
  #   ggplot(data = transformed_data_sum,
  #          aes(x = Time, y = Response, color = Treatment)) +
  #   geom_point() +
  #   geom_smooth(method = "lm", alpha = .15, aes(fill = Treatment))
  #   #geom_line(aes(color = Treatment, linetype = Treatment), size = 1.5) +
  #   # facet_wrap(scale ~ ., scales = "free_y") +
  #   theme_bw() +
  #   theme(legend.position = "bottom") +
  #   labs(color = "Treatment", linetype = "Treatment") +
  #   theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.75))

  # Cheng's suggestion 3/8/2022
  line_plot <- ggplot(transformed_data_sum, aes(
    x = Time, y = Mean_Response,
    color = Treatment, group = Treatment
  )) +
    geom_point(size = 1.25) +
    geom_line(
      aes(
        x = Time, y = Mean_Response,
        color = Treatment, linetype = Treatment
      ),
      size = 1.25,
      show.legend = F
    ) +
    geom_errorbar(aes(ymin = Mean_Response - sd_Response, ymax = Mean_Response + sd_Response),
      width = .5, show.legend = F
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(color = "Treatment", linetype = "Treatment") +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.75),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      strip.text = element_text(size = 14),
      title = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16)
    ) +
    ylab(ylabel) +
    ggtitle("Line Plot for Each Group Across Time Points") +
    guides(colour = guide_legend(override.aes = list(size = 10)))


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
