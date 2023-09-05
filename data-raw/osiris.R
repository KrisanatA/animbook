# The code needed to prepare the `osiris` dataset -------------------------


# Read the raw data -------------------------------------------------------

raw_data <- readr::read_csv("data-raw/osiris2021-sample1000.csv")


# Prepare the data --------------------------------------------------------

# select the relevant variable and remove 0 and NA value
data <- raw_data |>
  dplyr::select(year, country, ID, sales) |>
  dplyr::filter(!is.na(sales),
         sales > 0,
         dplyr::between(year, 2006, 2018))

# pull the company name (unique)
country <- data |>
  dplyr::select(ID, country) |>
  dplyr::distinct()

# complete the data using complete function for the missing year
osiris <- data |>
  dplyr::select(-country) |>
  tidyr::complete(year, ID) |>
  dplyr::left_join(country) |>
  dplyr::relocate(country, .after = ID) |>
  dplyr::mutate(year = as.integer(year),
         firmID = as.factor(ID),
         country = as.factor(country),
         sales = as.numeric(sales),
         japan = ifelse(country == "JP", "yes", "no"),
         japan = as.factor(japan))


usethis::use_data(osiris, overwrite = TRUE)

