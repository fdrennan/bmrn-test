
library(ggprism)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(ggpubr)

is.outlier <- function(x) {
  x < quantile(x, .25) - 1.5 * IQR(x) |
    x > quantile(x, .75) + 1.5 * IQR(x)
}


data <- read.csv("plot_ready_data.csv") %>%
  dplyr$filter(Time == "Day 1") %>%
  dplyr$group_by(Treatment) %>%
  dplyr$mutate(outlier = is.outlier(Response_Transformed)) %>%
  dplyr$ungroup()

# Load in p-values
load("trans_table.RData")

p_vals <- dplyr$bind_rows(tab1, tab2, tab3) %>%
  select(Treatment, `Times Included`, grep("p value from", colnames(.))) %>%
  tidyr$pivot_longer(cols = 3:ncol(.), names_to = "group2", values_to = "p value") %>%
  dplyr$rename(group1 = Treatment) %>%
  dplyr$filter(stats$complete.cases(.)) %>%
  dplyr$mutate(
    group2 = gsub("p value from ", "", group2),
    `p value` = dplyr$if_else(`p value` == "< 0.0001", "0.00001", `p value`),
    `p value` = as.numeric(`p value`),
    sig = dplyr$case_when(
      `p value` > 0.05 ~ "ns",
      `p value` <= 0.05 & `p value` > 0.01 ~ "*",
      `p value` <= 0.01 & `p value` > 0.001 ~ "**",
      `p value` <= 0.001 & `p value` > 0.0001 ~ "***",
      `p value` < 0.001 ~ "****"
    )
  ) %>%
  dplyr$filter(`p value` < 0.05) %>%
  dplyr$mutate(group2 = dplyr$case_when(
    group2 == "Dose 3" ~ "e13 300",
    group2 == "Dose 2" ~ "e13 200",
    group2 == "Vehicle" ~ "e13 empty-NP",
    group2 == "Negative Control" ~ "e13 no rap",
    group2 == "Wild Type" ~ "Wild Type"
  )) %>%
  dplyr$filter(`Times Included` == "Day 1") %>%
  dplyr$arrange(group2, group1) %>%
  dplyr$mutate(y.position = seq(1.25 * max(data$Response_Transformed),
    2.75 * max(data$Response_Transformed),
    length.out = nrow(.)
  ))


data$Treatment <- as.character(data$Treatment)
data$Treatment <- factor(data$Treatment, levels = unique(data$Treatment))
data$Treatment <- factor(data$Treatment, levels = c("Wild Type", "e13 empty-NP", "e13 no rap", "e13 100", "e13 200", "e13 300"))

response <- "Creatine Kinase (ng/ml)"
than

bottom <- full_prism +
  ggplot2$scale_y_continuous(limits = c(NA, 1.1 * max(data$Response_Transformed))) +
  ggplot2$theme(plot.margin = ggplot2$margin(t = -10, r = 0, b = 0, l = 0))

top <- full_prism +
  ggplot2$scale_y_continuous(limits = c(1.1 * min(p_vals$y.position), 3 * max(data$Response_Transformed))) +
  ggplot2$theme(
    line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  ggplot2$theme(plot.margin = ggplot2$margin(t = 0, r = 0, b = -10, l = 0))

final <- ggarrange(
  plotlist = list(top, bottom), ncol = 1,
  heights = c(2, 3)
)


annotate_figure(final, top = text_grob("Dive depths (m)",
  color = "red", face = "bold", size = 14
))
