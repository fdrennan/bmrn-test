#' final_model
#' @export
final_model <- function(transformed_data, best, variable) {
  box::use(dplyr)
  box::use(nlme)
  transformed_data <- dplyr$rename(transformed_data, tmp = variable)

  num_times <- nrw(dplyr$distinct(transformed_data, Time))

  # Autoregression correlation structure
  if (best == "AR1") {
    if (all(transformed_data$diff_group == "Pooled")) {
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = nlme$corAR1(form = ~ 1 | SubjectID)
      )
    } else {
      # The varIdent allows for a each group to have its own weight which is
      # computed based on the variance of the group
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = nlme$corAR1(form = ~ 1 | SubjectID),
        weights = nlme$varIdent(form = ~ 1 | diff_group)
      )
    }
  }

  # Autoregression correlation structure with heterogeneous variability across times
  if (best == "ARH1") {
    if (all(transformed_data$diff_group == "Pooled")) {
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = nlme$corAR1(form = ~ 1 | SubjectID),
        weights = nlme$varIdent(form = ~ 1 | Time)
      )
    } else {
      # The varIdent allows for a each group to have its own weight which is
      # computed based on the variance of the group
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = nlme$corAR1(form = ~ 1 | SubjectID),
        weights = nlme$varComb(
          nlme$varIdent(form = ~ 1 | diff_group),
          nlme$varIdent(form = ~ 1 | Time)
        )
      )
    }
  }

  # Compound Symmetry correlation structure
  if (best == "CS") {
    if (all(transformed_data$diff_group == "Pooled")) {
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = corCompSymm(form = ~ 1 | SubjectID)
      )
    } else {
      # The varIdent allows for a each group to have its own weight which is
      # computed based on the variance of the group
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = corCompSymm(form = ~ 1 | SubjectID),
        weights = nlme$varIdent(form = ~ 1 | diff_group)
      )
    }
  }

  # Compound Symmetry correlation structure with heterogeneous variability across times
  if (best == "CSH") {
    if (all(transformed_data$diff_group == "Pooled")) {
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = corCompSymm(form = ~ 1 | SubjectID),
        weights = nlme$varIdent(form = ~ 1 | Time)
      )
    } else {
      # The varIdent allows for a each group to have its own weight which is
      # computed based on the variance of the group
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = corCompSymm(form = ~ 1 | SubjectID),
        weights = nlme$varComb(nlme$varIdent(form = ~ 1 | diff_group), nlme$varIdent(form = ~ 1 | Time))
      )
    }
  }

  # Toeplitz correlation structure with heterogeneous variability across times
  if (best == "TOEP") {
    if (all(transformed_data$diff_group == "Pooled")) {
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = corARMA(p = num_times - 1, q = 0, form = ~ 1 | SubjectID),
      )
    } else {
      # The varIdent allows for a each group to have its own weight which is
      # computed based on the variance of the group
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = corARMA(p = num_times - 1, q = 0, form = ~ 1 | SubjectID),
        weights = nlme$varComb(nlme$varIdent(form = ~ 1 | diff_group), nlme$varIdent(form = ~ 1 | Time))
      )
    }
  }

  # Unstructured correlation structure
  if (best == "UN") {
    if (all(transformed_data$diff_group == "Pooled")) {
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = corSymm(form = ~ 1 | SubjectID),
        weights = nlme$varIdent(form = ~ 1 | Time)
      )
    } else {
      # The varIdent allows for a each group to have its own weight which is
      # computed based on the variance of the group
      final_mod <- nlme$gls(
        data = transformed_data,
        model = tmp ~ TreatmentNew * Time,
        correlation = corSymm(form = ~ 1 | SubjectID),
        weights = nlme$varComb(nlme$varIdent(form = ~ 1 | diff_group), nlme$varIdent(form = ~ 1 | Time))
      )
    }
  }

  return(final_mod)
}
