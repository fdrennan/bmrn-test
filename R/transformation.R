# This function first tests whether or not the data need to me be transformed
# using a Shapiro-Wilks Test. If this test has a significant p-value, then
# a Box COx transformation is conducted.

# This corresponds the top 2 boxes on the left of the flowchart. Note that is
# is only done for the basic model (vehicle and doses)

# Add in the linear model for shapiro wilk test, regress out the treatment effect
#' transformation_check
#' @export
transformation_check <- function(analysis_data) {
  box::use(dplyr)
  box::use(tidyr)
  box::use(stats)
  box::use(MASS)
  error_transform <- FALSE

  analysis_data <- analysis_data %>%
    dplyr$mutate(
      Response = as.numeric(Response),
      Baseline = as.numeric(Baseline)
    )
  # Compute the Subjects means to do Shapiro wilks test for only the basic model
  sub_mean <- dplyr$select(analysis_data, -Time)
  sub_mean <- tidyr$pivot_longer(sub_mean, cols = c(Response, Baseline), names_to = "Time", values_to = "Response") 
    sub_mean <- dplyr$mutate(sub_mean, Response = as.numeric(Response)) 
    sub_mean <- dplyr$filter(sub_mean, basic_model) 
    sub_mean <- dplyr$group_by(sub_mean, SubjectID, `Technical Replicate ID`, TreatmentNew) 
    sub_mean <- dplyr$summarize(sub_mean, sub_mean = mean(Response))
  model <- stats$lm(data = sub_mean, formula = 0 + sub_mean ~ TreatmentNew)
  # Conduct the Shapiro-Wilk Test. The aim of this step is to avoid unnecessary
  # data transformations.
  shapiro_wilk <- stats$shapiro.test(model$residuals)$p.value

  if (shapiro_wilk < 0.05) {
    # Subject means are not normally distributed, so we seek a transformation
    # that leads to normally distributed data

    # Dependent variable must be positive
    bc_data <-  dplyr$select(analysis_data, -Time) 
    bc_data <- tidyr$pivot_longer(bc_data, cols = c(Response, Baseline), names_to = "Time", values_to = "Response")
    parameter <- 0 # how much to shift the data
    if (min(bc_data$Response) <= 0) {
      # analysis_data$Response!=0 ensures that the data will be shifted
      # even if the minimum value is 0, and the 1.1 aims to make the shift relatively
      # small. In other words, the parameter won't be large compared to the scale of the
      # data
      parameter <- 1.1 * abs(min(as.numeric(bc_data$Response[bc_data$Response != 0])))
    }



    bc_data <- bc_data %>%
      dplyr$mutate(
        Response = as.numeric(Response),
        Response = Response + parameter
      )

    # Coonduct the box cox transformation
    bc <- MASS$boxcox(Response ~ TreatmentNew * Time,
      data = bc_data,
      lambda = seq(-1, 2, 1 / 100),
      plotit = FALSE
    )
    # Put boxcox summary data in a data.frame. We are only considering common
    # transformations i.e. inverse (-1), inverse sqrt (-1/2), log (0), sqrt (1/2),
    # identity (1), and squared (2) transformation. We select the tranformation
    # that leads to the largest log liklihood value.
    bc_loglik <- data.frame(lambda = bc$x, log_lik = bc$y)  
    bc_loglik <- dplyr$filter(bc_loglik, lambda %in% c(-1, -1 / 2, 0, 1 / 2, 1, 2)) 
    bc_loglik <- dplyr$arrange(bc_loglik, log_lik) 

    power <- bc_loglik$lambda
    message(paste("A power of", power, "was used to transform the data"))

    if (power == 0) {
      analysis_data <- analysis_data %>%
       dplyr$mutate(
          Baseline_Transformed = log(as.numeric(Baseline)) + parameter,
          Response_Transformed = log(as.numeric(Response) + parameter)
        )
    } else if (power == 1) {
      analysis_data <- analysis_data %>%
       dplyr$mutate(
          Baseline_Transformed = as.numeric(Baseline),
          Response_Transformed = as.numeric(Response)
        )
    } else {
      analysis_data <- analysis_data %>%
       dplyr$mutate(
          Baseline_Transformed = (as.numeric(Baseline) + parameter)^power,
          Response_Transformed = (as.numeric(Response) + parameter)^power
        )
    }

    # Conduct Shapiro-Wilks Test on transformed data to see if the subjects
    # means are normally distributed on the transformed scale.Again, this is only,
    # with the basic model

    sub_mean2 <- analysis_data %>%
     dplyr$select(-Time) %>%
      tidyr$pivot_longer(cols = c(Response_Transformed, Baseline_Transformed), names_to = "Time", values_to = "Response_Transformed") %>%
     dplyr$filter(basic_model) %>%
      dplyr$group_by(SubjectID, `Technical Replicate ID`, TreatmentNew) %>%
      dplyr$summarize((sub_mean = mean(Response_Transformed))

    model2 <- lm(data = sub_mean2, formula = 0 + sub_mean ~ TreatmentNew)
    shapiro_wilk2 <- shapiro.test(model2$residuals)$p.value
    if (shapiro_wilk2 < 0.05) {
      error_transform <- TRUE
    }
  } else {
    # The subject means were already normally distributed this there a transformation
    # is not needed
    message("No transformation required")
    # Regardless if a transformation was used, Response_Transformed will be on the
    # variable of interest for the rest of the analysis
    analysis_data <- analysis_data %>%
     dplyr$mutate(
        Response_Transformed = Response,
        Baseline_Transformed = Baseline
      )
    power <- 1
  }

  if (!all(is.na(analysis_data$Baseline))) {
    analysis_data <- analysis_data %>%
     dplyr$select(-`Technical Replicate ID`) %>%
      dplyr$group_by(SubjectID, Time, Type, Treatment, TypeNew, TreatmentNew, basic_model) %>%
      summarise(across(c(Response_Transformed, Baseline_Transformed, Baseline, Response), mean)) %>%
      ungroup() %>%
     dplyr$mutate(Response_Transformed_bc = Response_Transformed - Baseline_Transformed)
  } else {
    analysis_data <- analysis_data %>%
     dplyr$select(-`Technical Replicate ID`) %>%
      dplyr$group_by(Type, Treatment, SubjectID, Dose, TypeNew, TreatmentNew, basic_model, Time) %>%
      dplyr$group_by(SubjectID, Time, Type, Treatment, TypeNew, TreatmentNew, basic_model) %>%
      summarise(across(c(Response_Transformed, Baseline_Transformed, Baseline, Response), mean)) %>%
      ungroup()
  }


  return(list(transformed = analysis_data, bc_transformation = power, error_transform = error_transform))
}
