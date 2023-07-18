library(tidyverse)
library(ggplot2)
library(gganimate)


# simulated data ----------------------------------------------------------

n_com <- 100

scale <- 5

n <- 100

sim_data <- tibble(from = rep(2, n_com),
                   to = sample(1:4, n_com, replace = TRUE))


# function ----------------------------------------------------------------

sigmoid <- function(x_from, x_to, y_from, y_to, scale, n) {
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
         y = y * (y_to - y_from) + y_from)
}

linear <- function(x_from, x_to, y_from, y_to, n) {
  x <- seq(x_from, x_to, length = n)
  slope <- (y_from - y_to) / (x_from - x_to)
  y <- slope*x + y_from
  tibble(x = x,
         y = y)
}


# test with one observation -----------------------------------------------

linear(0, 1, as.numeric(sim_data[2, 1]), as.numeric(sim_data[2, 2]), n) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

a <- linear(0, 1, as.numeric(sim_data[2, 1]), as.numeric(sim_data[2, 2]), n) %>%
  mutate(time = row_number()) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  transition_time(time)

animate(a, nframes = n - 1)


# more observations -------------------------------------------------------

a2 <- map_dfr(seq_len(nrow(sim_data)),
             ~ linear(0, 1, as.numeric(sim_data[.x, 1]), as.numeric(sim_data[.x, 2]), n) %>%
               mutate(time = row_number() + .x,
                      y = y + runif(1, -0.1, 0.1))) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  transition_time(time)

animate(a2, nframes = n - 1)


# add counter -------------------------------------------------------------

data <- map_dfr(seq_len(nrow(sim_data)),
                ~ linear(0, 1, as.numeric(sim_data[.x, 1]), as.numeric(sim_data[.x, 2]), n) %>%
                  mutate(time = row_number() + .x,
                         y = y + runif(1, -0.1, 0.1),
                         id = .x) %>%
                  bind_cols(bind_rows(replicate(n, sim_data[.x, 2], simplify = FALSE))))

data <- map_dfr(seq_len(nrow(sim_data)),
                ~ sigmoid(0, 1, as.numeric(sim_data[.x, 1]), as.numeric(sim_data[.x, 2]), scale, n) %>%
                  mutate(time = row_number() + .x,
                         y = y + runif(1, -0.1, 0.1),
                         id = .x) %>%
                  bind_cols(bind_rows(replicate(n, sim_data[.x, 2], simplify = FALSE))))


# counter 1 ---------------------------------------------------------------

counter_1 <- data %>%
  group_by(id) %>%
  filter(to == 1) %>%
  summarise(time = max(time)) %>%
  count(time) %>%
  arrange(time) %>%
  mutate(n = cumsum(n),
         x = 0.5,
         y = 1,
         n = str_c("Counter number 1 has ", n, "observations"))

counter_f <- counter_1 %>%
  bind_rows(
    map_df(unique(data$time[data$time > max(counter_1$time)]),
           ~ slice(counter_1, nrow(counter_1)) %>%
             mutate(time = .x))
  )


# counter 2 ---------------------------------------------------------------

counter_2 <- data %>%
  group_by(id) %>%
  filter(to == 2) %>%
  summarise(time = max(time)) %>%
  count(time) %>%
  arrange(time) %>%
  mutate(n = cumsum(n),
         x = 0.5,
         y = 2,
         n = str_c("Counter number 2 has ", n, "observations"))

counter_s <- counter_2 %>%
  bind_rows(
    map_df(unique(data$time[data$time > max(counter_2$time)]),
           ~ slice(counter_2, nrow(counter_2)) %>%
             mutate(time = .x))
  )


# counter 3 ---------------------------------------------------------------

counter_3 <- data %>%
  group_by(id) %>%
  filter(to == 3) %>%
  summarise(time = max(time)) %>%
  count(time) %>%
  arrange(time) %>%
  mutate(n = cumsum(n),
         x = 0.5,
         y = 3,
         n = str_c("Counter number 3 has ", n, "observations"))

counter_t <- counter_3 %>%
  bind_rows(
    map_df(unique(data$time[data$time > max(counter_3$time)]),
           ~ slice(counter_3, nrow(counter_3)) %>%
             mutate(time = .x))
  )


# counter 4 ---------------------------------------------------------------

counter_4 <- data %>%
  group_by(id) %>%
  filter(to == 4) %>%
  summarise(time = max(time)) %>%
  count(time) %>%
  arrange(time) %>%
  mutate(n = cumsum(n),
         x = 0.5,
         y = 4,
         n = str_c("Counter number 4 has ", n, "observations"))

counter_fo <- counter_4 %>%
  bind_rows(
    map_df(unique(data$time[data$time > max(counter_4$time)]),
           ~ slice(counter_4, nrow(counter_4)) %>%
             mutate(time = .x))
  )

ending_box <- data %>%
  pull(to) %>%
  unique() %>%
  map_df(~ data.frame(x = c(1.01, 1.01, 1.1, 1.1, 1.01),
                      y = c(-0.3, 0.3, 0.3, -0.3, -0.3) + .x,
                      id = .x))


# animated with counter ---------------------------------------------------

a3 <- data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_text(data = counter_f, hjust = 0,
            aes(label = n, x = x, y = y), color = "black") +
  geom_text(data = counter_s, hjust = 0,
            aes(label = n, x = x, y = y), color = "black") +
  geom_text(data = counter_t, hjust = 0,
            aes(label = n, x = x, y = y), color = "black") +
  geom_text(data = counter_fo, hjust = 0,
            aes(label = n, x = x, y = y), color = "black") +
  theme_void() +
  transition_time(time)

animate(a3, nframes = n - 1)


# # compute change in sales -------------------------------------------------
#
# top_data_pct <- top_data %>%
#   group_by(firmID) %>%
#   mutate(pct_change = (sales/lead(sales) - 1) * 100) %>%
#   na.omit() %>%
#   select(-c(year, sales)) %>%
#   ungroup()
#
#
#
#
# # format draw data ------------------------------------------------------------
#
# draw <- top_data_pct_jp %>%
#   mutate(start = 0) %>%
#   relocate(start, .before = japan)
#
#   # draw end box
# min <- min(draw$pct_change)
#
# max <- max(draw$pct_change)
#
# box <- data.frame(x = c(1.01, 1.01, 1.05, 1.05, 1.01),
#                   y = c(min, max, max, min, min))
#
# # draw --------------------------------------------------------------------
#
# map <- map_dfr(seq_len(nrow(draw)),
#                ~ sigmoid(0, 1, as.numeric(draw[.x, 4]), as.numeric(draw[.x, 3]), scale, n) %>%
#                  mutate(time = row_number() + .x,
#                         id = .x) %>%
#   bind_cols(bind_rows(replicate(100, draw[.x, -(1:4)], simplify = FALSE))))
#
# t1 <- map %>%
#   ggplot(aes(x = x, y = y, color = japan)) +
#   geom_point() +
#   geom_path(data = box, aes(x, y), color = "grey70") +
#   guides(color = "none") +
#   transition_time(time)
#
# animate(t1, nframes = n - 1)




# first iteration ---------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(gganimate)
library(plotly)


raw_data <- read_csv("data-raw/osiris2021-sample1000.csv")

# only data from 2009 til 2018
data <- raw_data %>%
  filter(year >= 2009 & year < 2019) %>%
  select(year, country, firmID, sales) %>%
  filter(!is.na(sales))

# check the missing values
# naniar::vis_miss(data)

# only data with complete record (full 10 years data)
count_firm <- data %>%
  group_by(firmID) %>%
  count() %>%
  ungroup() %>%
  print()

n_year <- max(count_firm$n)

keep <- count_firm %>%
  filter(n == n_year)

complete_data <- data %>%
  filter(firmID %in% keep$firmID)

# rank the company based on sales for each year and filter top company
rank <- complete_data %>%
  group_by(year) %>%
  mutate(rank = rank(-sales),
         percentile = rank(-sales)/length(sales)) %>%
  ungroup()

# dummy variable for japan
rankj <- rank %>%
  mutate(japan = ifelse(country == "JP", 1, 0))

# select only top percentil company
keep_2 <- rankj %>%
  filter(year == 2009,
         rank <= 100) %>%
  pull(firmID)

top <- rankj %>%
  filter(firmID %in% keep_2)

# cut rank into 5 groups group 1 rank between 1-20, group 2 rank between 21-40,
# group 3 rank between 41-60, group 4 rank between 61-80, and group 5 rank between 81 or more

ctop <- top %>%
  mutate(start = case_when(
    rank > 80 ~ 1,
    rank > 60 ~ 2,
    rank > 40 ~ 3,
    rank > 20 ~ 4,
    rank >= 1 ~ 5
  ))

# check group
ctop %>%
  group_by(year, start) %>%
  summarise(n = n()) %>%
  print(n = 50)


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
           y_end = lead(year, n = 1)) %>%
    na.omit()

  map <- map_dfr(seq_len(nrow(data)),
                 ~ sigmoid(as.numeric(data[.x, 1]), as.numeric(data[.x, 9]),
                           as.numeric(data[.x, 7]), as.numeric(data[.x, 8]),
                           5, 700) %>%
                   mutate(id = .x)) %>%
    mutate(time = row_number())

  return(map)

}

more <- ctop %>%
  group_nest(firmID)

more2 <- more %>%
  mutate(draw = map(data, ~draw(.)))

jp <- rankj %>%
  select(firmID, japan) %>%
  group_by(firmID) %>%
  summarise(jp = mean(japan))

more3 <- more2 %>%
  select(firmID, draw) %>%
  unnest(cols = draw) %>%
  group_by(firmID) %>%
  mutate(y = y + runif(1, -0.1, 0.1),
         time = time + floor(runif(1, 100, 10000))) %>%
  left_join(jp, by = "firmID") %>%
  ungroup() %>%
  mutate(jp = as.factor(jp))

t2 <- more3 %>%
  ggplot(aes(x = x, y = y, color = jp)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1:5, linewidth = 5.5, alpha = 0.1) +
  scale_colour_discrete(name = "Japan",
                        breaks = c("1", "0"),
                        labels = c("Yes", "No")) +
  scale_x_continuous(breaks = seq(2009, 2018, 1)) +
  annotate("text", x = 2006.5, y = 5, label = "Rank 1 to 20") +
  annotate("text", x = 2006.5, y = 4, label = "Rank 21 to 40") +
  annotate("text", x = 2006.5, y = 3, label = "Rank 41 to 60") +
  annotate("text", x = 2006.5, y = 2, label = "Rank 61 to 80") +
  annotate("text", x = 2006.5, y = 1, label = "Rank 81 or more") +
  coord_cartesian(xlim = c(2009, 2018),
                  clip = 'off') +
  theme(aspect.ratio = 2/3,
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(1, 1, 1, 4, "cm")) +
  transition_time(time)

animate(t2, nframes = 1199, fps = 60)
