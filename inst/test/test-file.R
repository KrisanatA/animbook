library(tidyverse)
library(gganimate)
library(animbook)

# Toy dataset -------------------------------------------------------------

set.seed(29803829)

toy_dbl <- tibble(expand.grid(1:30, 2001:2010),
              values = as.double(runif(300, 1, 300)),
              group = as.factor(sample(c("apple", "samsung"), 300, replace = TRUE)),
              color = sample(c("1", "2", "3"), 300, replace = TRUE))

names(toy_dbl) <- c("id", "year", "values", "group", "color")

toy_dbl$id <- as.factor(toy_dbl$id)

# Osiris data -------------------------------------------------------------

full_data <- osiris |>
  filter(between(year, 2006, 2011))


# test code ---------------------------------------------------------------

# label
check <- c("Top 20%", "21-40", "41-60", "61-80", "81-100", "NA")

scale1 <- anim_prep(data = toy_dbl, id = id, values = values, time = year,
                    label = check, color = color)

scale2 <- anim_prep(data = toy_dbl, id = id, values = values, time = year,
                    label = check, group_scaling = group, color = color)

scale3 <- anim_prep(data = toy_dbl, id = id, values = values, time = year,
                    label = check, color = color, scaling = "absolute")

scale4 <- anim_prep(data = toy_dbl, id = id, values = values, time = year,
                    label = check, group_scaling = group, color = color,
                    scaling = "absolute")

anim_plot(scale2)

facet_plot(scale2)


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

order <- count(aes, party) |>
  arrange(n) |>
  pull(party)


p <- aes |>
  filter(id %in% liberal) |>
  arrange(id, year) |>
  group_by(id) |>
  mutate(
    frame = dplyr::row_number(),
    party = factor(party, levels = order)
  ) |>
  ungroup() |>
  ggplot() +
  geom_jitter(aes(x = year, y = party, group = id, color = gender), width = 0.2, height = 0.1) +
  gganimate::transition_time(frame)

gganimate::animate(p)



data <- prep_cat(aes, id, party, time = year)

p <- subset_plot(data, id, year, gender, label = rev(order))

p2 <- anim_animate(p)

gganimate::animate(p2)



# Absolute scale ----------------------------------------------------------

sales <- osiris$sales

cut(sales, breaks = pretty(sales, ngroup), include.lowest = TRUE, labels = seq(1, ngroup, 1))


min <- 1
max <- 20

v <- c(1, 5, 30)

dplyr::between(v, min, max)


# facet plot

data1 <- data |>
  filter(time > min(time)) |>
  group_by(id) |>
  mutate(facet = row_number())


data2 <- data |>
  filter(time < max(time)) |>
  group_by(id) |>
  mutate(facet = row_number())

full <- rbind(data1, data2) |>
  arrange(id, time)


full |>
  ggplot() +
  geom_line(aes(x = time, y = qtile, group = id, color = color),
            position = position_jitter(height = 0.2, width = 0)) +
  scale_x_continuous(breaks = scale1[["settings"]]$breaks) +
  facet_wrap(~facet, scales = "free")













