library(tidyverse)
library(gganimate)

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
  filter(year >= 2009 & year < 2019) |>
  mutate(japan = ifelse(country == "JP", "From Japan", "Not Japan"))



# test code ---------------------------------------------------------------


# label

check <- c("81-100", "61-80", "41-60", "21-40", "Top 20%")

check2 <- c("Not listed", "81-100", "61-80", "41-60", "21-40", "Top 20%")


# full step on toy data
data <- prep_anim(toy_dbl, id, rank)

p <- anim_plot(data, id, year, group, label = rev(check))
p

p2 <- anim_animate(p)
animate(p2, nframes = 2000, renderer = av_renderer())

# full step on osiris data
data2 <- prep_anim(full_data, firmID, sales, year)

os <- anim_plot(data2, firmID, year, japan, label = rev(check2))
os

os2 <- anim_animate(os)
gganimate::animate(os2, nframes = 10450, fps = 50, renderer = gifski_renderer("inst/test.gif"))


