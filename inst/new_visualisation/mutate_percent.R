library(tidyverse)
library(gganimate)
x <- read_csv("data-raw/osiris2021-sample1000.csv")

# filter 2009 - 2018 ------------------------------------------------------

data <- x %>%
  filter(year >= 2009 & year < 2019) %>%
  select(year, firmID, country, sales) %>%
  na.omit()

#naniar::vis_miss(data)

# complete record ---------------------------------------------------------

count_firm <- data %>%
  group_by(firmID, .drop = TRUE) %>%
  count()

count_firm %>%
  ungroup() %>%
  summarise(max(n))

keep <- count_firm %>%
  filter(n == 10)

data_2 <- data %>%
  filter(firmID %in% keep$firmID)
data_2

# start: compute percentile ------------------------------------------------------
ndata_2 <- data_2 |>
  group_nest(year)
ndata_2

ndata_3 <- ndata_2 |>
  mutate(rank = map(data, ~rank(-.$sales)),
         percentile = map(data, ~rank(.$sales)/length(.$sales)))
ndata_3

data_4 <- ndata_3 |>
  unnest(cols = c(data, rank, percentile))
data_4

p <- data_4 |>
  ggplot(aes(year, percentile, group = firmID, color = country)) +
  geom_line()
#
library(plotly)
ggplotly(p)
# end: compute percentile ------------------------------------------------------
