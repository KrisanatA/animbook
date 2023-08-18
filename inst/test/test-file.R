library(tidyverse)
library(gganimate)
library(animbook)

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

full_data <- osiris |>
  mutate(japan = ifelse(country == "JP", "From Japan", "Not Japan")) |>
  filter(between(year, 2006, 2011))


# test code ---------------------------------------------------------------

# label
check <- c("Top 20%", "21-40", "41-60", "61-80", "81-100", "Not listed", "test")


# full step on toy data
data <- prep_anim(toy_dbl, id, rank)

p <- anim_plot(data, id, year, group, label = check)
p

p2 <- anim_animate(p)
animate(p2, nframes = 2000, renderer = av_renderer())


# full step on osiris data
data2 <- prep_anim(full_data, firmID, sales, year, ngroup = 5)

os <- anim_plot(data2, firmID, year, japan, label = check, rendering = "plotly")
os

os2 <- anim_animate(os)
gganimate::animate(os2)


# Function example --------------------------------------------------------

set.seed(2)

gfc <- osiris |>
  filter(year == 2007 & !is.na(sales))

gfc2 <- osiris |>
  filter(year == 2008,
         firmID %in% gfc$firmID)

full_data <- as_tibble(rbind(gfc, gfc2))  |>
  mutate(japan = ifelse(country == "JP", "From Japan", "Not Japan"))

data <- prep_anim(full_data, firmID, sales, year, ngroup = 5, time_dependent = TRUE)

name_move <- data |>
  filter(year == 2007 & qtile != 0) |>
  pull(firmID)

name2_move <- data |>
  filter(year == 2008 & qtile == 0) |>
  pull(firmID)

name_nmove <- data |>
  mutate(lead = lead(qtile)) |>
  filter(qtile == lead) |>
  distinct(firmID) |>
  pull(firmID)

final_name_nmove <- sample(name_nmove, 5)


final_name_move <- sample(intersect(name_move, name2_move), 5)


prep_gfc <- prep_anim(full_data, firmID, sales, year, ngroup = 5, time_dependent = TRUE) |>
  filter(firmID %in% c(final_name_move, final_name_nmove))

p <- anim_plot(prep_gfc, firmID, year, japan, label = check, rendering = "gganimate")

p2 <- anim_animate(p)

animate(p2, nframes = 49)



# Explore plotly ----------------------------------------------------------

plotly::ggplotly(os)




# Category variable -------------------------------------------------------

liberal <- aes |>
  filter(year == 2016,
         party == "liberal") |>
  pull(id)



p <- aes |>
  filter(id %in% liberal) |>
  arrange(id, year) |>
  group_by(id) |>
  mutate(
    frame = dplyr::row_number()
  ) |>
  ungroup() |>
  ggplot() +
  geom_jitter(aes(x = year, y = party, group = id, color = gender), width = 0.2, height = 0.1) +
  gganimate::transition_time(frame)

gganimate::animate(p)
