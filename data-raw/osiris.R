# The code needed to prepare the `osiris` dataset -------------------------


# Library -----------------------------------------------------------------

library(tidyverse)


# Read the raw data -------------------------------------------------------

raw_data <- read_csv("data-raw/osiris2021-sample1000.csv")


# Prepare the data --------------------------------------------------------

# filter the data with sales record in selected period (2009 - 2018)
data <- raw_data |>
  filter(year >= 2009 & year < 2019) |>
  select(year, country, firmID, sales) |>
  filter(!is.na(sales) & sales != 0)

# pull the company name (unique)
country <- data |>
  select(firmID, country) |>
  distinct()

# complete the data using complete function for the missing year
osiris <- data |>
  select(-country) |>
  complete(year, firmID) |>
  left_join(country) |>
  mutate_if(is.numeric, ~replace_na(., 0)) |>
  relocate(country, .after = firmID)


usethis::use_data(osiris, overwrite = TRUE)
