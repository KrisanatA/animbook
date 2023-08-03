# The code needed to prepare the `osiris` dataset -------------------------


# Library -----------------------------------------------------------------

library(tidyverse)


# Read the raw data -------------------------------------------------------

raw_data <- read_csv("data-raw/osiris2021-sample1000.csv")


# Prepare the data --------------------------------------------------------

# select the relevant variable and remove 0 and NA value
data <- raw_data |>
  select(year, country, firmID, sales) |>
  filter(!is.na(sales),
         sales > 0)

# pull the company name (unique)
country <- data |>
  select(firmID, country) |>
  distinct()

# complete the data using complete function for the missing year
osiris <- data |>
  select(-country) |>
  complete(year, firmID) |>
  left_join(country) |>
  relocate(country, .after = firmID)


usethis::use_data(osiris, overwrite = TRUE)

