
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
  filter(Time == "Day 1") %>%
  group_by(Treatment) %>%
  mutate(outlier = is.outlier(Response_Transformed)) %>%
  ungroup()

# Load in p-values
load("trans_table.RData")

p_vals <- bind_rows(tab1, tab2, tab3) %>%
  select(Treatment, `Times Included`, grep("p value from", colnames(.))) %>%
  pivot_longer(cols = 3:ncol(.), names_to = "group2", values_to = "p value") %>%
  dplyr::rename(group1 = Treatment) %>%
  filter(complete.cases(.)) %>%
  mutate(
    group2 = gsub("p value from ", "", group2),
    `p value` = if_else(`p value` == "< 0.0001", "0.00001", `p value`),
    `p value` = as.numeric(`p value`),
    sig = case_when(
      `p value` > 0.05 ~ "ns",
      `p value` <= 0.05 & `p value` > 0.01 ~ "*",
      `p value` <= 0.01 & `p value` > 0.001 ~ "**",
      `p value` <= 0.001 & `p value` > 0.0001 ~ "***",
      `p value` < 0.001 ~ "****"
    )
  ) %>%
  filter(`p value` < 0.05) %>%
  mutate(group2 = case_when(
    group2 == "Dose 3" ~ "e13 300",
    group2 == "Dose 2" ~ "e13 200",
    group2 == "Vehicle" ~ "e13 empty-NP",
    group2 == "Negative Control" ~ "e13 no rap",
    group2 == "Wild Type" ~ "Wild Type"
  )) %>%
  filter(`Times Included` == "Day 1") %>%
  arrange(group2, group1) %>%
  mutate(y.position = seq(1.25 * max(data$Response_Transformed),
    2.75 * max(data$Response_Transformed),
    length.out = nrow(.)
  ))


data$Treatment <- as.character(data$Treatment)
data$Treatment <- factor(data$Treatment, levels = unique(data$Treatment))
data$Treatment <- factor(data$Treatment, levels = c("Wild Type", "e13 empty-NP", "e13 no rap", "e13 100", "e13 200", "e13 300"))

response <- "Creatine Kinase (ng/ml)"
than

bottom <- full_prism +
  scale_y_continuous(limits = c(NA, 1.1 * max(data$Response_Transformed))) +
  theme(plot.margin = margin(t = -10, r = 0, b = 0, l = 0))

top <- full_prism +
  scale_y_continuous(limits = c(1.1 * min(p_vals$y.position), 3 * max(data$Response_Transformed))) +
  theme(
    line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  theme(plot.margin = margin(t = 0, r = 0, b = -10, l = 0))

final <- ggarrange(
  plotlist = list(top, bottom), ncol = 1,
  heights = c(2, 3)
)


annotate_figure(final, top = text_grob("Dive depths (m)",
  color = "red", face = "bold", size = 14
))
