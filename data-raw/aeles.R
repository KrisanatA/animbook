# The code needed to prepare the `aeles` dataset ----------------------------


# Read the raw data -------------------------------------------------------

raw_data <- readr::read_csv("data-raw/aes19_unrestricted.csv")


# Prepare the data --------------------------------------------------------

aeles <- raw_data |>
  janitor::clean_names() |>
  dplyr::select(id, panel_flag, b9_1, b14, h1) |>
  # filter only the same person
  dplyr::filter(panel_flag == 1) |>
  # only have the survey answer (removed irrelevant answer)
  dplyr::filter(dplyr::between(b9_1, 1, 6),
                dplyr::between(b14, 1, 6),
         h1 %in% c(1, 2, 3)) |>
  # format the variable
  dplyr::mutate(gender = dplyr::case_when(
           h1 == 1 ~ "male",
           h1 == 2 ~ "female",
           h1 == 3 ~ "other"
         ),
         vote_2019 = dplyr::case_when(
           b9_1 == 1 ~ "liberal",
           b9_1 == 2 ~ "labor",
           b9_1 == 3 ~ "national",
           b9_1 == 4 ~ "greens",
           b9_1 == 5 ~ "other",
           b9_1 == 6 ~ "not vote"
         ),
         vote_2016 = dplyr::case_when(
           b14 == 1 ~ "liberal",
           b14 == 2 ~ "labor",
           b14 == 3 ~ "national",
           b14 == 4 ~ "greens",
           b14 == 5 ~ "other",
           b14 == 6 ~ "not vote"
         ),
         .keep = "unused") |>
  tidyr::pivot_longer(c(vote_2019, vote_2016),
               names_to = "year",
               values_to = "party") |>
  dplyr::mutate(year = as.integer(stringr::str_extract(year, "(\\d+)"))) |>
  dplyr::select(id, year, party, gender) |>
  dplyr::mutate(id = as.factor(id),
                party = as.factor(party))







usethis::use_data(aeles, overwrite = TRUE)
