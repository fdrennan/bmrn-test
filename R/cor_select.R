#' cor_select
#' @export

cor_select <- function(transformed_data, variable) {
  print(variable)
  num_times <- transformed_data %>%
    distinct(Time) %>%
    nrow()
  # Try AR1, CS, corSymm (unstructured), corExp (SP) correlations structures
  # and let user know which one is 'best' based on smallest AIC

  AR1 <- gls(
    data = transformed_data %>% filter(basic_model),
    model = as.formula(paste(variable, "~ TreatmentNew * Time")),
    correlation = corAR1(form = ~ 1 | SubjectID)
  )
  print(AR1)
  aic_AR1 <- c("AR1" = -2*logLik(AR1)[1] + 2*2)
  ARH1 <- gls(
    data = transformed_data %>% filter(basic_model),
    model = as.formula(paste(variable, "~ TreatmentNew * Time")),
    correlation = corAR1(form = ~ 1 | SubjectID),
    weights = varIdent(form = ~ 1 | Time)
  )
  aic_ARH1 <- c("ARH1" =  -2*logLik(ARH1)[1] +
                  2*(2 + length(levels(transformed_data$Time))))
  CS <- gls(
    data = transformed_data %>% filter(basic_model),
    model = as.formula(paste(variable, "~ TreatmentNew * Time")),
    correlation = corCompSymm(form = ~ 1 | SubjectID)
  )
  print(CS)
  aic_CS <- c("CS" = -2*logLik(CS)[1] + 2*2)
  CSH <- gls(
    data = transformed_data %>% filter(basic_model),
    model = as.formula(paste(variable, "~ TreatmentNew * Time")),
    correlation = corCompSymm(form = ~ 1 | SubjectID),
    weights = varIdent(form = ~ 1 | Time)
  )
  aic_CSH <- c("CSH" = -2*logLik(CSH)[1] +
                 2*(2 + length(levels(transformed_data$Time))))
  TOEP <- gls(
    data = transformed_data %>% filter(basic_model),
    model = as.formula(paste(variable, "~ TreatmentNew * Time")),
    correlation = corARMA(p = num_times - 1, q = 0, form = ~ 1 | SubjectID),
    weights = varIdent(form = ~ 1 | Time)
  )
  aic_TOEP <- c("TOEP" = -2*logLik(TOEP)[1] +
                  2 * length(levels(transformed_data$Time)))
  UN <- try(gls(
    data = transformed_data %>% filter(basic_model),
    model = as.formula(paste(variable, "~ TreatmentNew * Time")),
    correlation = corSymm(form = ~ 1 | SubjectID),
    weights = varIdent(form = ~ 1 | Time)
  ))
  aic_UN <- c("UN" = ifelse(class(UN) == "try-error", NA, AIC(UN)))

  aic <- c(aic_AR1, aic_ARH1, aic_CS, aic_CSH, aic_TOEP, aic_UN)
  print(aic)
  message(paste("The best correlation structure is", names(aic[which.min(aic)])))

  return(names(aic[which.min(aic)]))
}
