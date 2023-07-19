rank <- osiris |>
  group_by(year) |>
  mutate(rank = as.integer(rank(-sales)),
         percentile = rank(-sales)/length(sales)) |>
  ungroup()

full_data <- osiris |>
  filter(year >= 2009 & year < 2019) |>
  left_join(rank) |>
  mutate_if(is.numeric, ~replace_na(., 0)) |>
  mutate(pos = case_when(
    percentile > 0.8 ~ 1,
    percentile > 0.6 ~ 2,
    percentile > 0.4 ~ 3,
    percentile > 0.2 ~ 4,
    percentile > 0 ~ 5,
    percentile == 0 ~ 0
  ),
  japan = ifelse(country == "JP", "Yes", "No")) |>
  select(-country)
