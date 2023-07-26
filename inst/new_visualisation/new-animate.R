# library -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(gganimate)


raw_data <- read_csv("inst/new_visualisation/osiris2021-sample1000.csv")


# filter the data with sales record in selected period (2009 - 2018)
data <- raw_data %>%
  filter(year >= 2009 & year < 2019) %>%
  select(year, country, firmID, sales) %>%
  filter(!is.na(sales) & sales != 0)

# pull the company name (unique)
country <- data %>%
  select(firmID, country) %>%
  distinct()

# complete the data using complete function for the missing year
complete_data <- data %>%
  select(-country) %>%
  tidyr::complete(year, firmID) %>%
  left_join(country)

# rank the sales
rank <- data %>%
  group_by(year) %>%
  mutate(rank = as.integer(rank(-sales)),
         percentile = rank(-sales)/length(sales)) %>%
  ungroup()

# count_rank <- rank %>%
#   group_by(rank, year) %>%
#   count() %>%
#   ungroup() %>%
#   print
#
# count_rank %>%
#   filter(n != 1)
#
# rank %>%
#   filter(year == 2011) %>%
#   distinct(rank) %>%
#   arrange(rank) %>%
#   print(n = 400)

# split the rank into groups
full_data <- complete_data %>%
  left_join(rank) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate(start = case_when(
    percentile > 0.8 ~ 1,
    percentile > 0.6 ~ 2,
    percentile > 0.4 ~ 3,
    percentile > 0.2 ~ 4,
    percentile > 0 ~ 5,
    percentile == 0 ~ 0
  ),
  japan = ifelse(country == "JP", "Yes", "No")) %>%
  select(-country)

# data for tick mark

x <- unique(full_data$year)

y <- unique(full_data$start) - 0.28

tick <- expand.grid(x, y) |>
  mutate(id = row_number(),
         y2 = Var2 - 0.15) |>
  rename(x = Var1,
         y = Var2) |>
  pivot_longer(c(y, y2),
               values_to = "y",
               names_to = "yy") |>
  select(-yy)

tick |>
  ggplot(aes(x = x, y = y, group = id)) +
  geom_line()



# function ----------------------------------------------------------------

sigmoid <- function(x_from, x_to, y_from, y_to, scale, n) {
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
         y = y * (y_to - y_from) + y_from)
}

draw <- function(df) {

  data <- df %>%
    arrange(year) %>%
    mutate(end = lead(start, n = 1),
           year_end = lead(year, n = 1)) %>%
    na.omit()

  map <- map_dfr(seq_len(nrow(data)),
                 ~ sigmoid(as.numeric(data[.x, 1]), as.numeric(data[.x, 8]),
                           as.numeric(data[.x, 5]), as.numeric(data[.x, 7]),
                           5, 100) %>%
                   mutate(id = .x)) %>%
    mutate(time = row_number())

  return(map)
}


# draw --------------------------------------------------------------------

map <- full_data %>%
  group_nest(firmID) %>%
  mutate(draw = map(data, ~draw(.)))

jp <- full_data %>%
  select(firmID, japan) %>%
  group_by(firmID) %>%
  distinct()

map <- map %>%
  select(firmID, draw) %>%
  unnest(cols = draw) %>%
  group_by(firmID) %>%
  mutate(y = y + runif(1, -0.1, 0.1),
         time = time + floor(runif(1, 100, 10000))) %>%
  left_join(jp, by = "firmID") %>%
  ungroup() %>%
  mutate(japan = as.factor(japan))


# counter -----------------------------------------------------------------

counter1 <- map |>
  filter(round(y, 0) == 5,
         x == max(x),
         id == 9) |>
  group_by(time) |>
  mutate(n_time = n()) |>
  arrange(time) |>
  ungroup() |>
  mutate(n = cumsum(n_time),
         x = x + 1,
         y = round(y, 0),
         japan = ifelse(japan == "Yes", 1, 0),
         n_japan = cumsum(japan),
         per_notjp = round(((n - n_japan)/n)*100, 0),
         per_jp = 100-per_notjp,
         per_notjp = str_c(per_notjp, "%"),
         per_jp = str_c(per_jp, "%")) |>
  select(x, y, time, per_notjp, per_jp) |>
  complete(time = seq(1, max(map$time))) |>
  arrange(time) |>
  fill(x, y, per_notjp, per_jp) |>
  mutate(x = 2019,
         y = 5,
         per_notjp = ifelse(is.na(per_notjp), paste(0, "%"), per_notjp),
         per_jp = ifelse(is.na(per_jp), paste(0, "%"), per_jp))


counter2 <- map |>
  filter(round(y, 0) == 4,
         x == max(x),
         id == 9) |>
  group_by(time) |>
  mutate(n_time = n()) |>
  arrange(time) |>
  ungroup() |>
  mutate(n = cumsum(n_time),
         x = x + 1,
         y = round(y, 0),
         japan = ifelse(japan == "Yes", 1, 0),
         n_japan = cumsum(japan),
         per_notjp = round(((n - n_japan)/n)*100, 0),
         per_jp = 100-per_notjp,
         per_notjp = str_c(per_notjp, "%"),
         per_jp = str_c(per_jp, "%")) |>
  select(x, y, time, per_notjp, per_jp) |>
  complete(time = seq(1, max(map$time))) |>
  arrange(time) |>
  fill(x, y, per_notjp, per_jp) |>
  mutate(x = 2019,
         y = 4,
         per_notjp = ifelse(is.na(per_notjp), paste(0, "%"), per_notjp),
         per_jp = ifelse(is.na(per_jp), paste(0, "%"), per_jp))

counter3 <- map |>
  filter(round(y, 0) == 3,
         x == max(x),
         id == 9) |>
  group_by(time) |>
  mutate(n_time = n()) |>
  arrange(time) |>
  ungroup() |>
  mutate(n = cumsum(n_time),
         x = x + 1,
         y = round(y, 0),
         japan = ifelse(japan == "Yes", 1, 0),
         n_japan = cumsum(japan),
         per_notjp = round(((n - n_japan)/n)*100, 0),
         per_jp = 100-per_notjp,
         per_notjp = str_c(per_notjp, "%"),
         per_jp = str_c(per_jp, "%")) |>
  select(x, y, time, per_notjp, per_jp) |>
  complete(time = seq(1, max(map$time))) |>
  arrange(time) |>
  fill(x, y, per_notjp, per_jp) |>
  mutate(x = 2019,
         y = 3,
         per_notjp = ifelse(is.na(per_notjp), paste(0, "%"), per_notjp),
         per_jp = ifelse(is.na(per_jp), paste(0, "%"), per_jp))

counter4 <- map |>
  filter(round(y, 0) == 2,
         x == max(x),
         id == 9) |>
  group_by(time) |>
  mutate(n_time = n()) |>
  arrange(time) |>
  ungroup() |>
  mutate(n = cumsum(n_time),
         x = x + 1,
         y = round(y, 0),
         japan = ifelse(japan == "Yes", 1, 0),
         n_japan = cumsum(japan),
         per_notjp = round(((n - n_japan)/n)*100, 0),
         per_jp = 100-per_notjp,
         per_notjp = str_c(per_notjp, "%"),
         per_jp = str_c(per_jp, "%")) |>
  select(x, y, time, per_notjp, per_jp) |>
  complete(time = seq(1, max(map$time))) |>
  arrange(time) |>
  fill(x, y, per_notjp, per_jp) |>
  mutate(x = 2019,
         y = 2,
         per_notjp = ifelse(is.na(per_notjp), paste(0, "%"), per_notjp),
         per_jp = ifelse(is.na(per_jp), paste(0, "%"), per_jp))

counter5 <- map |>
  filter(round(y, 0) == 1,
         x == max(x),
         id == 9) |>
  group_by(time) |>
  mutate(n_time = n()) |>
  arrange(time) |>
  ungroup() |>
  mutate(n = cumsum(n_time),
         x = x + 1,
         y = round(y, 0),
         japan = ifelse(japan == "Yes", 1, 0),
         n_japan = cumsum(japan),
         per_notjp = round(((n - n_japan)/n)*100, 0),
         per_jp = 100-per_notjp,
         per_notjp = str_c(per_notjp, "%"),
         per_jp = str_c(per_jp, "%")) |>
  select(x, y, time, per_notjp, per_jp) |>
  complete(time = seq(1, max(map$time))) |>
  arrange(time) |>
  fill(x, y, per_notjp, per_jp) |>
  mutate(x = 2019,
         y = 1,
         per_notjp = ifelse(is.na(per_notjp), paste(0, "%"), per_notjp),
         per_jp = ifelse(is.na(per_jp), paste(0, "%"), per_jp))

counter6 <- map |>
  filter(round(y, 0) == 0,
         x == max(x),
         id == 9) |>
  group_by(time) |>
  mutate(n_time = n()) |>
  arrange(time) |>
  ungroup() |>
  mutate(n = cumsum(n_time),
         x = x + 1,
         y = round(y, 0),
         japan = ifelse(japan == "Yes", 1, 0),
         n_japan = cumsum(japan),
         per_notjp = round(((n - n_japan)/n)*100, 0),
         per_jp = 100-per_notjp,
         per_notjp = str_c(per_notjp, "%"),
         per_jp = str_c(per_jp, "%")) |>
  select(x, y, time, per_notjp, per_jp) |>
  complete(time = seq(1, max(map$time))) |>
  arrange(time) |>
  fill(x, y, per_notjp, per_jp) |>
  mutate(x = 2019,
         y = 0,
         per_notjp = ifelse(is.na(per_notjp), paste(0, "%"), per_notjp),
         per_jp = ifelse(is.na(per_jp), paste(0, "%"), per_jp))

counter <- bind_rows(counter1, counter2, counter3, counter4, counter5, counter6)


# gganimate ---------------------------------------------------------------

p <- ggplot() +
  geom_point(data = map, aes(x = x, y = y, color = japan), size = 2) +
  geom_hline(yintercept = 0:5, linewidth = 5.6, alpha = 0.1) +
  geom_text(data = counter, hjust = 0,
            aes(label = per_notjp, x = x + 1.4, y = y), color = "blue") +
  geom_text(data = counter, hjust = 0,
            aes(label = per_jp, x = x, y = y), color = "red") +
  annotate("text", x = 2006.5, y = 5, label = "Top 20%") +
  annotate("text", x = 2006.5, y = 4, label = "21 - 40") +
  annotate("text", x = 2006.5, y = 3, label = "41 - 60") +
  annotate("text", x = 2006.5, y = 2, label = "61 - 80") +
  annotate("text", x = 2006.5, y = 1, label = "81 - 100") +
  annotate("text", x = 2006.5, y = 0, label = "Not listed") +
  scale_colour_manual(name = "",
                      breaks = c("Yes", "No"),
                      labels = c("From Japan", "Not from Japan"),
                      values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(2009, 2018, 1)) +
  coord_cartesian(xlim = c(2009, 2021),
                  clip = 'off') +
  theme(aspect.ratio = 2/3,
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(1, 1, 1, 4, "cm"),
        legend.position = "bottom") +
  geom_line(data = tick, aes(x = x, y = y, group = id)) +
  transition_time(time)

# change nframes to 99 and remove fps for a faster animate time
animate(p)


# grey bar, write a function (colour), potential play button,
# description in the title or caption

# non standard evaluation, enter string as a variable, factor, counters, animation is an add on after the plot

# documentation: inspiration for the plot, gganimate, steps, application of the
# plot


# plotly ------------------------------------------------------------------

figly <- ctop %>%
  group_by(firmID) %>%
  mutate(start = start + runif(1, -0.3, 0.3),
         yearn = year + runif(1, -0.3, 0.3))

fig <- figly %>%
  plotly::plot_ly(
    x = ~yearn,
    y = ~start,
    color = ~as.factor(japan),
    frame = ~year,
    text = ~firmID,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )

fig <- fig %>% animation_opts(1500)

fig


# Conditional proportion --------------------------------------------------

# Test with 2 year

test <- full_data |>
  filter(year %in% c(2009:2010)) |>
  group_by(firmID) |>
  arrange(year) |>
  mutate(end = lead(start, n = 1),
         year_end = lead(year, n = 1),
         joint = paste0(as.character(start), " then ", as.character(end))) |>
  na.omit() |>
  ungroup() |>
  count(year, joint, start)

test |>
  group_by(start) |>
  mutate(prop = n/sum(n)) |>
  print(n = 40)


# Full_data

prop <- full_data |>
  group_by(firmID) |>
  arrange(year) |>
  mutate(end = lead(start, n = 1),
         joint = paste0(as.character(start), " then ", as.character(end)),
         p = paste0("P(", as.character(end), "|", as.character(start), ")")) |>
  na.omit() |>
  ungroup() |>
  count(year, start, p) |>
  group_by(year, start) |>
  mutate(prop = n/sum(n))


# without sigmoid path ----------------------------------------------------


set.seed(123)

p2 <- full_data |>
  group_by(firmID) |>
  mutate(time = row_number(),
         time = time + floor(runif(1, 10, 100))) |>
  arrange(firmID) |>
  ggplot() +
  geom_jitter(aes(x = year, y = start, group = firmID, color = japan)) +
  geom_hline(yintercept = 0:5, linewidth = 5.6, alpha = 0.1) +
  annotate("text", x = 2007.5, y = 5, label = "Top 20%") +
  annotate("text", x = 2007.5, y = 4, label = "21 - 40") +
  annotate("text", x = 2007.5, y = 3, label = "41 - 60") +
  annotate("text", x = 2007.5, y = 2, label = "61 - 80") +
  annotate("text", x = 2007.5, y = 1, label = "81 - 100") +
  annotate("text", x = 2007.5, y = 0, label = "Not listed") +
  scale_colour_manual(name = "",
                      breaks = c("Yes", "No"),
                      labels = c("From Japan", "Not from Japan"),
                      values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(2009, 2018, 1)) +
  coord_cartesian(xlim = c(2009, 2018),
                  clip = 'off') +
  theme(aspect.ratio = 2/3,
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(1, 1, 1, 4, "cm"),
        legend.position = "bottom") +
  geom_line(data = tick, aes(x = x, y = y, group = id)) +
  transition_time(time)

animate(p2)

