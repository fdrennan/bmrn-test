# This function estimates the adjusted confidence intervals and p-values based
# on a single set of tests. Here the adjust = 'mvt' corresponds to the simulate
# option in proc glimmix SAS which simulates potential test statistics from a
# multivariate t-distribution with accounts for the correlation and the degrees
# of freedom.

#' @export
final_contrasts <- function(model, cont_list, est, letter, overall_trend = FALSE) {
  box::use(dplyr)
  box::use(emmeans)
  box::use(stats)


  adj_table <- data.frame(
    letter = LETTERS[1:12],
    p.adj = c(
      "none", "none", "none", "dunnett", "none",
      "none", "mvt", "none", "dunnett", "none", "none", "none"
    )
  )

  if (overall_trend) {
    adjust <- "mvt"
  } else {
    adjust <- adj_table$p.adj[adj_table$letter == letter]
  }

  if (length(cont_list) > 0) {
    # Compute the simulated p-value
    # browser()
    adj_ht <- emmeans$contrast(est, adjust = adjust, method = cont_list)
    adj_ht <- as.data.frame(adj_ht)

    # Compute confidence interval for the the contrast
    adj_ci <- stats$confint(emmeans$contrast(est, adjust = adjust, method = cont_list))
    adj_ci <- as.data.frame(adj_ci)

    # Some rounding, this can be done here or when results are output in excel
    # It might make for sense to do it later, if we are able to use the scientific
    # notation in excel

    results <- dplyr$inner_join(adj_ht, adj_ci)

    results <- dplyr$mutate(results,
      estimate = round(estimate, 2),
      SE = round(SE, 3),
      df = round(df, 2),
      t.ratio = round(t.ratio, 2),
      p.value = round(p.value, 3),
      lower.CL = round(lower.CL, 3),
      upper.CL = round(upper.CL, 3)
    )
    return(results)
  }
}
