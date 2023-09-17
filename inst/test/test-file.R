library(animbook)

# Toy dataset -------------------------------------------------------------

set.seed(29803829)

toy_dbl <- tibble::tibble(expand.grid(1:30, 2001:2010),
              values = as.double(runif(300, 1, 300)),
              group = as.factor(sample(c("apple", "samsung"), 300, replace = TRUE)),
              color = sample(c("1", "2", "3"), 300, replace = TRUE))

names(toy_dbl) <- c("id", "year", "values", "group", "color")

toy_dbl$id <- as.factor(toy_dbl$id)

# # Osiris data -------------------------------------------------------------
#
# full_data <- osiris |>
#   filter(between(year, 2006, 2011))


# test code ---------------------------------------------------------------

# label
check <- c("Top 20%", "21-40", "41-60", "61-80", "81-100", "NA")


# toy dataset
scale1 <- anim_prep(data = toy_dbl, id = id, values = values, time = year,
                    label = check, color = color, time_dependent = FALSE)

scale2 <- anim_prep(data = toy_dbl, id = id, values = values, time = year,
                    label = check, group_scaling = group, color = color)

scale3 <- anim_prep(data = toy_dbl, id = id, values = values, time = year,
                    label = check, color = color, scaling = "absolute")

scale4 <- anim_prep(data = toy_dbl, id = id, values = values, time = year,
                    label = check, group_scaling = group, color = color,
                    scaling = "absolute")

p <- anim_plot(scale1)

facet_plot(scale1)

p2 <- anim_animate(p)

gganimate::animate(p2, nframes = 200)

# osiris dataset
scale1 <- anim_prep(data = osiris |> dplyr::mutate(sales = log(sales)),
                    id = ID, values = sales, time = year,
                    label = check,
                    color = japan, scaling = "absolute")



# Function example --------------------------------------------------------

# set.seed(2)
#
# gfc <- osiris |>
#   filter(year == 2007 & !is.na(sales))
#
# gfc2 <- osiris |>
#   filter(year == 2008,
#          ID %in% gfc$ID)
#
# full_data <- as_tibble(rbind(gfc, gfc2))  |>
#   mutate(japan = ifelse(country == "JP", "From Japan", "Not Japan"))
#
# data <- prep_anim(full_data, ID, sales, year, ngroup = 5, time_dependent = TRUE)
#
# name_move <- data |>
#   filter(year == 2007 & qtile != 0) |>
#   pull(ID)
#
# name2_move <- data |>
#   filter(year == 2008 & qtile == 0) |>
#   pull(ID)
#
# name_nmove <- data |>
#   mutate(lead = lead(qtile)) |>
#   filter(qtile == lead) |>
#   distinct(ID) |>
#   pull(ID)
#
# final_name_nmove <- sample(name_nmove, 5)
#
#
# final_name_move <- sample(intersect(name_move, name2_move), 5)
#
#
# prep_gfc <- prep_anim(full_data, ID, sales, year, ngroup = 5, time_dependent = TRUE) |>
#   filter(ID %in% c(final_name_move, final_name_nmove))
#
# p <- anim_plot(prep_gfc, ID, year, japan, label = check, rendering = "gganimate")
#
# p2 <- anim_animate(p)
#
# animate(p2, nframes = 49)


# Explore plotly ----------------------------------------------------------

plotly::ggplotly(os)


# Category variable -------------------------------------------------------

liberal <- aeles |>
  filter(year == 2016,
         party == "liberal") |>
  pull(id)

order <- count(aeles, party) |>
  arrange(n) |>
  pull(party)


p <- aeles |>
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


aeles <- aeles |>
  mutate(id = as.factor(id),
         party = as.factor(party),
         year = as.integer(year))


data <- anim_prep_cat(aeles, id, party, time = year, color = gender)

p <- anim_plot(data, plot = "wallaby", width = 0.01)

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





animbook <- anim_prep(data = osiris, id = ID, values = sales, time = year, color = japan)

p <- anim_plot(animbook, plot = "wallaby")

p2 <- anim_animate(p)

animate(p2, nframes = 29)





# subset with sankey ------------------------------------------------------

test11 <- tibble::tibble(id = rep(1:4, each = 2),
                       prob = rep(c(0.4, 0.2, 0.2, 0.2), each = 2),
                       mid = prob/2,
                       x = 0,
                       y = 4 - cumsum(mid) + mid) |>
  dplyr::group_by(id) |>
  dplyr::slice_head(n = 1)

test12 <- tibble::tibble(id = rep(1:4, each = 2),
                       prob = rep(c(0.4, 0.2, 0.2, 0.2), each = 2),
                       mid = prob/2,
                       x = 0,
                       y = 4 - cumsum(mid) - mid) |>
  dplyr::group_by(id) |>
  dplyr::slice_head(n = 1)

test21 <- tibble::tibble(id = rep(1:4, each = 2),
                        prob = rep(c(0.4, 0.2, 0.2, 0.2), each = 2),
                        mid = prob/2,
                        x = 1,
                        y = 4 - cumsum(mid) + mid) |>
  dplyr::group_by(id) |>
  dplyr::slice_head(n = 1)

test22 <- tibble::tibble(id = rep(1:4, each = 2),
                         prob = rep(c(0.4, 0.2, 0.2, 0.2), each = 2),
                         mid = prob/2,
                         x = 1,
                         y = 4 - cumsum(mid) - mid) |>
  dplyr::group_by(id) |>
  dplyr::slice_head(n = 1)

test31 <- tibble::tibble(id = rep(1:4, each = 2),
                        prob = rep(c(0.4, 0.2, 0.2, 0.2), each = 2),
                        mid = prob/2,
                        x = 9,
                        y = rep(4:1, each = 2)) |>
  dplyr::group_by(id) |>
  dplyr::slice_head(n = 1)

test32 <- tibble::tibble(id = rep(1:4, each = 2),
                         prob = rep(c(0.4, 0.2, 0.2, 0.2), each = 2),
                         mid = prob/2,
                         x = 9,
                         y = rep(4:1, each = 2) - prob) |>
  dplyr::group_by(id) |>
  dplyr::slice_head(n = 1)

test41 <- tibble::tibble(id = rep(1:4, each = 2),
                         prob = rep(c(0.4, 0.2, 0.2, 0.2), each = 2),
                         mid = prob/2,
                         x = 10,
                         y = rep(4:1, each = 2)) |>
  dplyr::group_by(id) |>
  dplyr::slice_head(n = 1)

test42 <- tibble::tibble(id = rep(1:4, each = 2),
                         prob = rep(c(0.4, 0.2, 0.2, 0.2), each = 2),
                         mid = prob/2,
                         x = 10,
                         y = rep(4:1, each = 2) - prob) |>
  dplyr::group_by(id) |>
  dplyr::slice_head(n = 1)


test <- rbind(test11, test21, test31, test41, test42, test32, test22, test12)

test |>
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, group = id)) +
  ggplot2::geom_polygon()



wallaby <- wallaby_data(data)

test <- wallaby[["data"]]


prob <- c(0.4, 0.2, 0.3, 0.1)

left <- tibble::tibble(id = 1:6,
                       xstart = 0,
                       ystart = 6)

right <- tibble::tibble(id = 1:6,
                        xend = 10,
                        yend = 1:6)

full <- left |>
  dplyr::left_join(right,
            by = "id")


lookup <- test |>
  dplyr::filter(time == 1) |>
  dplyr::count(qtile) |>
  mutate(prob = n/sum(n),
         len = prob/2) |>
  select(qtile, len)


test1 <- tibble::tibble(id = rep(1:4, each = 2),
                        prob = rep(prob, each = 2),
                        mid = prob/2,
                        xstart = 0,
                        ystart = 4 - cumsum(mid)) |>
  dplyr::group_by(id) |>
  dplyr::slice_head(n = 1)

test2 <- tibble::tibble(id = rep(1:4),
                         prob = rep(prob),
                         mid = prob/2,
                         xend = 1,
                         yend = rep(4:1) - mid)

full <- test1 |>
  dplyr::left_join(test2,
            by = c("id", "prob", "mid"))

map <- purrr::map_dfr(seq_len(nrow(full)),
               ~ sigmoid(as.numeric(full[.x, 2]), as.numeric(full[.x, 4]),
                         as.numeric(full[.x, 3]), as.numeric(full[.x, 5]),
                         5) |>
                 dplyr::mutate(id = .x))

table <- map |>
  dplyr::left_join(lookup,
                   by = c("id" = "qtile")) |>
  na.omit()

split <- split(table, f = table$id)

ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = split[[1]], ggplot2::aes(x = x, y = y, ymin = y - len, ymax = y + len), alpha = 0.1) +
  ggplot2::geom_ribbon(data = split[[2]], ggplot2::aes(x = x, y = y, ymin = y - len, ymax = y + len), alpha = 0.1) +
  ggplot2::geom_ribbon(data = split[[3]], ggplot2::aes(x = x, y = y, ymin = y - len, ymax = y + len), alpha = 0.1) +
  ggplot2::geom_ribbon(data = split[[4]], ggplot2::aes(x = x, y = y, ymin = y - len, ymax = y + len), alpha = 0.1) +
  ggplot2::geom_ribbon(data = split[[5]], ggplot2::aes(x = x, y = y, ymin = y - len, ymax = y + len), alpha = 0.1)




# sankey algorithm --------------------------------------------------------

initial <- 3

prop <- c(0.4, 0.3, 0.1, 0.2)

prop2 <- c(0.5, 0.3, 0.2, 0.1)

top <- vector()

bottom <- vector()

for (i in 1:length(prop)) {
  top[i] <- initial

  bottom[i] <- initial - prop[i]

  initial <- initial - prop[i]
}


left <- tibble::tibble(ystart = c(top, bottom),
                       xstart = 0) |>
  dplyr::arrange(desc(ystart)) |>
  dplyr::mutate(id = dplyr::row_number())

right <- tibble::tibble(yend = c(rev(1:length(prop)), (rev(1:length(prop)) - prop)),
                        xend = 1) |>
  dplyr::arrange(desc(yend)) |>
  dplyr::mutate(id = dplyr::row_number())

full <- left |>
  dplyr::left_join(right,
                   by = "id")


map <- purrr::map_dfr(seq_len(nrow(full)),
                      ~ sigmoid(as.numeric(full[.x, 2]), as.numeric(full[.x, 5]),
                                as.numeric(full[.x, 1]), as.numeric(full[.x, 4]),
                                10) |>
                        dplyr::mutate(id = .x))

split <- split(map, map$id)


for (i in seq(2, 8, by = 2)) {
  split[[i]] <- split[[i]] |>
    dplyr::arrange(desc(x))

  split[[i-1]]$id <- i - 1
  split[[i]]$id <- i - 1
}


split

data <- do.call("rbind", split)

library(ggplot2)

ggplot(data = data, aes(x = x, y = y, group = id, fill = as.factor(id))) +
  geom_polygon(alpha = 0.1) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank())







# -------------------------------------------------------------------------




object <- anim_prep_cat(aeles, id, party, year)

data <- object[["data"]]




subset <- data |>
  dplyr::filter(time == min(time),
                qtile == 6) |>
  dplyr::pull(id)

subset_data <- data |>
  dplyr::filter(id %in% subset) |>
  dplyr::mutate(time = dplyr::case_when(time == min(time) ~ 0,
                                        time == max(time) ~ 1))


subset_data |>
  dplyr::count(qtile) |>
  dplyr::mutate(prop = n/sum(n),
                prop = ifelse(prop < 0.1, 0.1, prop))

osiris |>
  dplyr::filter(ID %in% subset) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = sales, group = ID)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymax = sales, ymin = sales - 100000), fill = "blue") +
  ggplot2::geom_line()

data <- anim_prep(osiris, id = ID, values = sales, time = year)




point0 <- tibble::tibble(
  id = unique(data[["data"]]$qtile),
  x = 0,
  y = min(data[["data"]]$time)
)


