#' generate_data
#' @export generate_data

generate_data <- function(group_specs, num_times, num_tech_reps,
                          transform = 1, correlation = "ar", rho = 0.5, intercept = 5) {
  data <- lapply(unique(group_specs$Treatment), function(i) {
    sub <- c(t(replicate(num_tech_reps, 1:group_specs$n[group_specs$Treatment == i])))
    tech <- rep(1:num_tech_reps, group_specs$n[group_specs$Treatment == i])
    trend <- group_specs$trend[group_specs$Treatment == i]
    group_variance <- group_specs$group_variance[group_specs$Treatment == i]
    group_name <- group_specs$Treatment[group_specs$Treatment == i]
    num_times <- num_times + 1 # Include a baseline measurement

    if (!(transform %in% c(0, 1))) {
      group_variance <- group_variance * transform * trend^(2 * (transform - 1))
      trend <- trend^transform
    }
    if (transform == 0) {
      group_variance <- (exp(group_variance) - 1) * exp(2 * trend + group_variance)
      trend <- exp(trend + group_variance / 2)
    }

    if (correlation == "ar") {
      cor <- diag(num_times)
      for (a in 1:num_times) {
        for (b in 1:num_times) {
          cor[a, b] <- rho^(abs(a - b))
        }
      }
      cov <- diag(x = group_variance, nrow = num_times) %*% cor %*%
        diag(x = group_variance, nrow = num_times)
    }
    if (correlation == "cs") {
      cor <- diag(num_times)
      cor[cor != 1] <- rho
      cov <- diag(x = group_variance, nrow = num_times) %*% cor %*%
        diag(x = group_variance, nrow = num_times)
    }
    if (correlation == "un") {
      # Generate a random positive definite matrix
      A <- matrix(runif(num_times^2) * 2 - 1, ncol = num_times)
      Sigma <- t(A) %*% A
      cor <- cov2cor(Sigma)
      cov <- diag(x = group_variance, nrow = num_times) %*% cor %*%
        diag(x = group_variance, nrow = num_times)
    }

    map2_dfr(
      .x = sub, .y = tech,
      .f = ~ {
        tmp <- mvtnorm::rmvnorm(n = 1, mean = rep(0, num_times), sigma = cov)
        ts.sim <- intercept + trend * 1:num_times + tmp
        if (transform == 0) {
          ts.sim <- exp(ts.sim)
        }

        if (abs(transform) == 1 / 2) {
          ts.sim <- abs(ts.sim)^transform
        }

        if (abs(transform) != 1 / 2 & transform != 0) {
          ts.sim <- ts.sim^transform
        }

        out <- data.frame(SubjectID = .x, Technical = .y, Treatment = i, ts.sim)
        colnames(out) <- gsub(
          pattern = "X", replacement = "Time ",
          x = colnames(out)
        )
        out
      }
    )
  }) %>%
    dplyr$bind_rows() %>%
    dplyr$rename(
      `Technical Replicate ID` = "Technical",
      `Treatment Group Name` = "Treatment"
    ) %>%
    dplyr$mutate(
      Type = ifelse(`Treatment Group Name` == "Wild Type",
        "Wild Type", "Non-Wild Type"
      ),
      Dose = as.numeric(gsub(
        "[A-z]| ", "",
        `Treatment Group Name`
      )),
      Baseline = `Time 1`,
      `Treatment Group Name` = gsub(
        pattern = "Dose.*", replacement = "Treatment",
        x = `Treatment Group Name`
      )
    ) %>%
    dplyr$select(-`Time 1`) %>%
    dplyr$arrange(`Treatment Group Name`) %>%
    dplyr$select(
      Type, `Treatment Group Name`, SubjectID, `Technical Replicate ID`, Dose,
      Baseline, grep(pattern = "Time", x = colnames(.), value = TRUE)
    )
  data
}
