library(test)

norm_check = function(data){
  data_long <- data %>%
    filter(basic_model) %>%
    pivot_longer(cols = grep("Time", x = colnames(.)), names_to = "Time", values_to = "Response")

  model <- lm(Response ~ `Treatment Group Name` * Time, data = data_long)
  shapiro.test(residuals(model))$p.value
}

boxcox = function(data){
  data_long <- data %>%
    filter(basic_model) %>%
    pivot_longer(cols = grep("Time", x = colnames(.)), names_to = "Time", values_to = "Response")

  min_obs <- min(data_long$Response)
  data_long_bc <- data_long

  if (min_obs < 0) {
    data_long_bc <- data_long_bc %>%
      mutate(Response = Response - 1.1 * min_obs)
  }

  bc <- MASS::boxcox(Response ~ `Treatment Group Name` * Time,
                     data = data_long_bc,
                     lambda = seq(-1, 2, 1 / 100))
  tmp = data.frame(power = c(-1, -1 / 2, 0, 1 / 2, 1, 2), loglik = bc$y[bc$x %in% c(-1, -1 / 2, 0, 1 / 2, 1, 2)])
  power = tmp$power[which.max(tmp$loglik)]
}


lrt <- function(data, basic_model) {
  if (basic_model) {
    data <- data %>%
      filter(basic_model) %>%
      mutate(`Treatment Group Name` = ifelse(`Treatment Group Name` == "Treatment", paste("Treatment", Dose),
        `Treatment Group Name`
      ))
  } else {
    data <- data %>%
      mutate(`Treatment Group Name` = ifelse(basic_model, "basic_model", `Treatment Group Name`))
  }

  full_model <- gls(
    model = Response ~ Treatment * Time,
    data = data %>% rename("Treatment" = `Treatment Group Name`),
    weights = varIdent(form = ~ 1 | Treatment)
  )

  reduced <- gls(
    model = Response ~ Treatment * Time,
    data = data %>% rename("Treatment" = `Treatment Group Name`)
  )

  anova(full_model, reduced)["reduced", "p-value"] < 0.05
}

checks <- function(data, group_specs, cfb) {

  data_merged <- data %>%
    inner_join(group_specs %>%
      mutate(Treatment = gsub("Dose.*", "Treatment", Treatment)),
    by = c("Treatment" = "Treatment Group Name"), .
    ) %>%
    rename("Treatment Group Name" = "Treatment")


  # Normality check
  normality = norm_check(data_merged)
  if(normality < 0.05){
  min_obs <- min(data[,grep('Time',colnames(data))])
  bc_results = boxcox(data_merged)
  if(bc_results == 0){
    data_merged[,grep('Time',colnames(data))] = log(data_merged[,grep('Time',colnames(data))]+ min_obs*1.1*(min_obs<0))
  }else{
    data_merged[,grep('Time',colnames(data))] = (data_merged[,grep('Time',colnames(data))] + min_obs*1.1*(min_obs<0))^bc_results
  }}

  # variance check
  if (cfb) {
    data_merged <- data_merged %>%
      mutate_at(.vars = grep('Time', colnames(.),value = TRUE),
                ~. - Baseline)
  }

  data_long = data_merged %>%
    pivot_longer(cols = grep("Time", x = colnames(.)), names_to = "Time", values_to = "Response")

  var_check <- data_long %>%
    group_by(Type, `Treatment Group Name`, Dose, SubjectID, Time) %>%
    summarize(Response = mean(Response)) %>%
    group_by(Type, `Treatment Group Name`, Dose, Time) %>%
    summarize(var = var(Response)) %>%
    group_by(Type, `Treatment Group Name`, Dose) %>%
    summarize(var = mean(var))

  var_check_basic <- data_long %>%
    filter(basic_model) %>%
    group_by(Type, `Treatment Group Name`, Dose, SubjectID, Time) %>%
    summarize(Response = mean(Response)) %>%
    group_by(Type, `Treatment Group Name`, Dose, Time) %>%
    summarize(var = var(Response)) %>%
    group_by(Type, `Treatment Group Name`, Dose) %>%
    summarize(var = mean(var))

  lrt_basic <- lrt(data_long, basic_model = T)
  lrt_controls <- lrt(data_long, basic_model = F)

  return(list(
    var = var_check, pooled_var = mean(var_check_basic$var), lrt_basic_sig = lrt_basic, lrt_controls_sig = lrt_controls,
    bc = bc_results)
  )
}



# Example 1
num_times <- 5

group_name <- c(
  "Vehicle", "Positive Control", "Negative Control", "Wild Type",
  "Dose 5", "Dose 4", "Dose 3", "Dose 2", "Dose 1"
)
basic_model <- c(T, F, F, F, T, T, T, T, T)
trend <- -c(1, 1, 1, 1, 1, 1, 1, 1, 1)
group_variance <- c(1, 2, 1, 1, 1, 1, 1, 1, 1)
n <- c(5,5, 5, 5, 5, 5, 5, 5, 5)*2

group_specs <- cbind.data.frame(
  Treatment = group_name,
  trend = trend,
  group_variance = group_variance,
  n = n,
  basic_model = basic_model
)
num_tech_reps <- 3
set.seed(12)
data <- generate_data(
  group_specs = group_specs,
  num_times = num_times,
  num_tech_reps = num_tech_reps,
  transform = 1,
  correlation = "ar",
  rho = 3 / 4
)


checks(data = data, group_specs = group_specs, cfb = F)

data %>% write.csv('../Test_Report/tmp.csv')

# Example 2 Change Posttive Control to Vehicle and remove the Original Vehicle group
num_times <- 5

group_name <- c(
  "Vehicle", "Positive Control", "Negative Control", "Wild Type",
  "Dose 5", "Dose 4", "Dose 3", "Dose 2", "Dose 1"
)
basic_model <- c(T, F, F, F, T, T, T, T, T)
trend <- -c(1, 0.9, 1, 1, 1, 1, 1, 1, 1)
group_variance <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
n <- c(5, 10, 10, 7, 5, 5, 5, 5, 5)

group_specs <- cbind.data.frame(
  Treatment = group_name,
  trend = trend,
  group_variance = group_variance,
  n = n,
  basic_model = basic_model
)
num_tech_reps <- 3
set.seed(123)
data <- generate_data(
  group_specs = group_specs,
  num_times = num_times,
  num_tech_reps = num_tech_reps,
  transform = 0,
  rho = 3 / 4,
  intercept = 0
)

data %>% write.csv('../Test_Report/tmp.csv')


# Example 3
num_times <- 5

group_name <- c(
  "Vehicle", "Positive Control", "Negative Control", "Wild Type",
  "Dose 5", "Dose 4", "Dose 3", "Dose 2", "Dose 1"
)
basic_model <- c(T, F, F, F, T, T, T, T, T)
trend <- -c(1, 1, 1, 1, 1, 1, 1, 1, 1)
group_variance <- c(1, 1, 1, 1, 1, 1, 1, 1.25, 1)
n <- c(5,5, 5, 5, 5, 5, 5, 5, 5)

group_specs <- cbind.data.frame(
  Treatment = group_name,
  trend = trend,
  group_variance = group_variance,
  n = n,
  basic_model = basic_model
)
num_tech_reps <- 3
set.seed(123)
data <- generate_data(
  group_specs = group_specs,
  num_times = num_times,
  num_tech_reps = num_tech_reps,
  transform = 1,
  correlation = "ar",
  rho = 3 / 4
)

data %>% write.csv('../Test_Report/tmp.csv')

# Example 4
num_times <- 5

group_name <- c(
  "Vehicle", "Positive Control", "Negative Control", "Wild Type",
  "Dose 5", "Dose 4", "Dose 3", "Dose 2", "Dose 1"
)
basic_model <- c(T, F, F, F, T, T, T, T, T)
trend <- -c(1, 1, 1, 1, 1, 1, 1, 1, 1)
group_variance <- c(1, 1, 1, 1, 1, 1, 1, 4, 1)
n <- c(5,5, 5, 5, 5, 5, 5, 5, 5)

group_specs <- cbind.data.frame(
  Treatment = group_name,
  trend = trend,
  group_variance = group_variance,
  n = n,
  basic_model = basic_model
)
num_tech_reps <- 3
set.seed(123)
data <- generate_data(
  group_specs = group_specs,
  num_times = num_times,
  num_tech_reps = num_tech_reps,
  transform = -1,
  correlation = "ar",
  rho = 3 / 4
)

data[grep('Time',colnames(data))] = exp(data[grep('Time',colnames(data))])

data %>% write.csv('../Test_Report/tmp.csv')



# Example 5
num_times <- 5

group_name <- c(
  "Vehicle", "Positive Control", "Negative Control", "Wild Type",
  "Dose 5", "Dose 4", "Dose 3", "Dose 2", "Dose 1"
)
basic_model <- c(T, F, F, F, T, T, T, T, T)
trend <- c(1, -0.1, -0.5, 0, -0.5, -0.4, -0.35, 1, 1)
group_variance <- c(1.25, 1, 1, 1, 1, 1, 1, 1, 1)
n <- c(5, 5, 5, 5, 5, 5, 5, 5, 5)*2

group_specs <- cbind.data.frame(
  Treatment = group_name,
  trend = trend,
  group_variance = group_variance,
  n = n,
  basic_model = basic_model
)
num_tech_reps <- 3
set.seed(123456)
data <- generate_data(
  group_specs = group_specs,
  num_times = num_times,
  num_tech_reps = num_tech_reps,
  transform = 1,
  correlation = "ar",
  rho = 3 / 4
)


checks(data = data, group_specs = group_specs, cfb = T)

bc_var = data %>%
  pivot_longer(cols = grep(pattern = 'Time', x = colnames(.), value = TRUE),
               names_to = "Time", values_to = "Response") %>%
  mutate(Response = Response - Baseline,
         basic_model = ifelse(!is.na(Dose)|`Treatment Group Name` == 'Vehicle',
                              T, F)) %>%
  group_by(Type, `Treatment Group Name`, Dose, Time, SubjectID, basic_model) %>%
  summarize(Response = mean(Response)) %>%
  group_by(Type, `Treatment Group Name`, Dose, Time, basic_model) %>%
  summarize(var = var(Response)) %>%
  group_by(Type, `Treatment Group Name`, Dose, basic_model) %>%
  summarize(mean_var = mean(var))

bc_var
bc_var %>% filter(basic_model) %>% ungroup() %>% summarize(mean(mean_var))


no_bc_var = data %>%
  pivot_longer(cols = grep(pattern = 'Time', x = colnames(.), value = TRUE),
               names_to = "Time", values_to = "Response") %>%
  mutate(basic_model = ifelse(!is.na(Dose)|`Treatment Group Name` == 'Vehicle',
                              T, F)) %>%
  group_by(Type, `Treatment Group Name`, Dose, Time, SubjectID, basic_model) %>%
  summarize(Response = mean(Response)) %>%
  group_by(Type, `Treatment Group Name`, Dose, Time, basic_model) %>%
  summarize(var = var(Response)) %>%
  group_by(Type, `Treatment Group Name`, Dose, basic_model) %>%
  summarize(mean_var = mean(var))

no_bc_var
no_bc_var %>% filter(basic_model) %>% ungroup() %>% summarize(mean(mean_var))


data %>% write.csv('../Test_Report/tmp.csv')
