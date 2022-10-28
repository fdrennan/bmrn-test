# This function checks that variance of the groups in the basic model is similar
# This corresponds to box fifth in the flow chart
variance_check <- function(transformed_data, variable) {
  # First find the variance for each TreatmentNew and week combination,
  # then take the average of these variances to estimate the variance
  # for each group
  tmp <- transformed_data
  variances <- transformed_data %>%
    group_by(TreatmentNew, basic_model, Time) %>%
    summarize(var = var(get(variable))) %>%
    group_by(TreatmentNew, basic_model) %>%
    summarize(mean_var = mean(var))

  # We really want to make sure that the variance for the groups in the basic model
  # is similar. So we compare their variances to the mean (pooled) variance of these
  # groups. There will be two conditions to indicate that variances are not similar
  # 1. larger than three fold change from the pooled variance of the basic_model, or
  # 2. larger than two fold change and a significant p-value from the log likelihood

  # Log liklihood test, comparing a model with a one overall variance for the basic
  # model (reduced model) and a model estimating the variances separately (full model)

  full_model <- gls(
    model = as.formula(paste(variable, "~ TreatmentNew * Time")), ,
    data = transformed_data,
    weights = varIdent(form = ~ 1 | basic_model)
  )

  restricted_model <- gls(
    model = as.formula(paste(variable, "~ TreatmentNew * Time")), ,
    data = transformed_data
  )

  pooled_var <- variances %>%
    filter(basic_model) %>%
    ungroup() %>%
    summarize(pooled = mean(mean_var))

  loglik_diff <- -2 * (restricted_model$logLik - full_model$logLik)
  df_diff <- attributes(loglik_diff)
  p_value <- 1 - pchisq(as.numeric(loglik_diff), df_diff$df)

  if (p_value < 0.05) {
    # If the weighted model (full model) has a significantly better fit then the
    # model without weights (reduced model), then we will consider a group variance
    # that is 2 fold change from the pooled variance. Here I look at the log 2
    # fold change meaning that every integer increase change corresponds to leads
    # to a increase of at 2 in the ratio. In other words, a log2 fold of -3 change means
    # that the pooled variance is 8 times the group variance
    variances <- variances %>%
      mutate(
        var_ratio = mean_var / pooled_var$pooled,
        fold_change = log(var_ratio, base = 2),
        diff_group = if_else(abs(fold_change) > 1, as.character(TreatmentNew), "Pooled")
      ) %>%
      ungroup() %>%
      select(-basic_model)
  } else {
    # If the weighted model (full model) has a not significantly better fit then the
    # model without weights (reduced model), then there is no benefit of estimating
    # weights fo each group. Here I look at the log 3
    # fold change meaning that every integer increase change corresponds to leads
    # to a increase of at 3 in the ratio. In other words, a log3 fold of -3 change means
    # that the pooled variance is 27 times the group variance
    variances <- variances %>%
      mutate(
        var_ratio = mean_var / pooled_var$pooled,
        fold_change = log(var_ratio, base = 3),
        diff_group = if_else(abs(fold_change) <= 1, "Pooled", as.character(TreatmentNew))
      ) %>%
      ungroup() %>%
      select(-basic_model)
  }

  transformed_data <- transformed_data %>%
    left_join(variances)



  # Output a new dataset with diff_var column which indicates the groupings
  # for the final model
  return(transformed_data)
}
