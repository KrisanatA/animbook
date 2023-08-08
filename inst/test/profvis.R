library(tidyverse)
library(animbook)

full_data <- osiris |>
  filter(year >= 2009 & year < 2019) |>
  mutate(japan = ifelse(country == "JP", "From Japan", "Not Japan"))

check2 <- c("Not listed", "81-100", "61-80", "41-60", "21-40", "Top 20%")

data2 <- prep_anim(full_data, firmID, sales, year)

os <- anim_plot(data2, firmID, year, japan, label = rev(check2))
os

os2 <- anim_animate(os)
gganimate::animate(os2)
