box::use(./connections/postgres)
box::use(dplyr[tbl, collect, group_by, mutate,
               ungroup, n,
               summarise, filter, select, distinct, arrange, desc])
box::use(lubridate[month, day, week])
con <- postgres$connection_postgres()

tbl(con, 'transactions') |>
  collect() |>
  mutate(
    month = month(date)
  ) |>
  filter(month>5) |>
  group_by(month, description) |>
  summarise(n = n(), amount = paste0(unique(amount))) |>
  filter(n <= 2) |>
  group_by(description) |>
  mutate(n_obs = n()) |>
  ungroup() |>
  distinct(description, n_obs, amount) |>
  arrange(desc(n_obs)) |>
  as.data.frame()
