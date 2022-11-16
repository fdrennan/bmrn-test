#' @export
variance_test_basic <- function(transformed_data, variable) {
  {
    box::use(dplyr)
    box::use(nlme)
    box::use(stats)
  }
  error_bm_var <- FALSE

  orig_data <- transformed_data
  # First find the variance for each TreatmentNew and week combination,
  # then take the average of these variances to estimate the variance
  # for each group
  variances <- dplyr$filter(transformed_data, basic_model)
  variances <- dplyr$group_by(variances, TreatmentNew, Time)
  variances <- dplyr$summarize(variances, var = stats$var(get(variable)))
  variances <- dplyr$group_by(variances, TreatmentNew)
  variances <- dplyr$summarize(variances, mean_var = mean(var))

  pooled_var <- dplyr$ungroup(variances)
  pooled_var <- dplyr$summarize(pooled_var, pooled = mean(mean_var))

  # We really want to make sure that the variance for the groups in the basic model
  # is similar. So we compare their variances to the mean (pooled) variance of these
  # groups. There will be two conditions to indicate that variances are not similar
  # 1. larger than three fold change from the pooled variance of the basic_model, or
  # 2. larger than two fold change and a significant p-value from the log likelihood

  # Log liklihood test, comparing a model with a one overall variance for the basic
  # model (reduceed model) and a model estimating the variances separately (full model)

  full_model <- nlme$gls(
    model = stats$as.formula(paste(variable, "~ TreatmentNew * Time")),
    data = dplyr$filter(transformed_data, basic_model),
    weights = nlme$varIdent(form = ~ 1 | TreatmentNew)
  )

  restricted_model <- nlme$gls(
    model = stats$as.formula(paste(variable, "~ TreatmentNew * Time")),
    data = dplyr$filter(transformed_data, basic_model)
  )

  loglik_diff <- -2 * (restricted_model$logLik - full_model$logLik)
  df_diff <- attributes(loglik_diff)
  p_value <- 1 - stats$pchisq(as.numeric(loglik_diff), df_diff$df)


  if (p_value < 0.05) {
    variances <-
      dplyr$mutate(
        variances,
        var_ratio = mean_var / pooled_var$pooled,
        fold_change = log(var_ratio, base = 2),
        diff_group = dplyr$if_else(abs(fold_change) > 1, "Yes", "Pooled")
      )
  } else {
    variances <-
      dplyr$mutate(
        variances,
        var_ratio = mean_var / pooled_var$pooled,
        fold_change = log(var_ratio, base = 3),
        diff_group = dplyr$if_else(abs(fold_change) > 1, "Yes", "Pooled")
      )
  }

  transformed_data <- dplyr$left_join(transformed_data, variances)

  if (any(variances$diff_group == "Yes")) {
    error_bm_var <- TRUE
  } else {
    message("The within group Variances are similar in the basic model")
  }
  return(list(data = orig_data, error_bm_var = error_bm_var))
}
