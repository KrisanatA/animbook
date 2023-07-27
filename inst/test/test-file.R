library(tidyverse)

# Toy dataset -------------------------------------------------------------

set.seed(29803829)

rank_dbl <- as.double(runif(100, 1, 100))
rank_int <- as.integer(runif(100, 1, 100))
rank_fac <- as.factor(floor(runif(100, 1, 6)))

toy_dbl <- tibble(expand.grid(1:30, 2001:2010),
              rank = sample(rank_dbl, 300, replace = TRUE),
              group = sample(c("apple", "samsung"), 300, replace = TRUE))
names(toy_dbl) <- c("id", "year", "rank", "group")

toy_int <- tibble(expand.grid(1:30, 2001:2010),
                  rank = sample(rank_int, 300, replace = TRUE),
                  group = sample(c("apple", "samsung"), 300, replace = TRUE))
names(toy_int) <- c("id", "year", "rank", "group")

toy_fac <- tibble(expand.grid(1:30, 2001:2010),
                  rank = sample(rank_fac, 300, replace = TRUE),
                  group = sample(c("apple", "samsung"), 300, replace = TRUE))
names(toy_fac) <- c("id", "year", "rank", "group")


# Osiris data -------------------------------------------------------------

rank <- osiris |>
  group_by(year) |>
  mutate(rank = as.integer(rank(-sales)),
         percentile = rank(-sales)/length(sales)) |>
  ungroup()

full_data <- osiris |>
  filter(year >= 2009 & year < 2019) |>
  left_join(rank) |>
  mutate_if(is.numeric, ~replace_na(., 0)) |>
  mutate(japan = ifelse(country == "JP", "From Japan", "Not Japan")) |>
  # CIC holdings limited NACY19950502 country is not known, and based on the sales number, I will be removing this firm
  na.omit() |>
  select(-country)

