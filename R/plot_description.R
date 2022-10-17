# plot_description <- function(change_from_baseline, transformed_scale) {
#   if (transformed_scale) {
#     scale <- "transformed"
#   } else {
#     scale <- "original"
#   }
#
#   if (change_from_baseline) {
#     analysis <- "change from baseline"
#   } else {
#     analysis <- "no shift from baseline"
#   }
#
#   col1 <- c(
#     "Box plot (top left)",
#     "Bar plot (bottom left)",
#     "Group level line plot (top right)",
#     "Subject level line plot (bottom right)"
#   )
#
#   col2 <- c(
#     "Black dots correspond to the group mean.",
#     "Side-by-side bar plots by time point with observed values and standard error bars.",
#     "Mean trajectory of each group with standard errot bars at each time point.",
#     "Trajectory for each subject across each time point."
#   )
#
#   tab <- cbind.data.frame(
#     "Plot" = col1,
#     "Description" = col2
#   )
#   return(tab)
# }
