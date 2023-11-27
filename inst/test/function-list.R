library(animbook)
library(tidyverse)

# osiris ------------------------------------------------------------------

label <- c("Top20", "20-40", "40-60", "60-80", "80-100", "Not listed")

osiris1 <- anim_prep(data = osiris,
                     id = ID,
                     values = sales,
                     time = year,
                     label = label,
                     ncat = 5L,
                     group = japan)

osiris2 <- anim_prep(data = osiris,
                     id = ID,
                     values = sales,
                     time = year,
                     label = label,
                     ncat = 5L,
                     group_scaling = country,
                     group = japan)

osiris3 <- anim_prep(data = osiris,
                     id = ID,
                     values = sales,
                     time = year,
                     ncat = 5L,
                     group = japan,
                     scaling = "absolute")

osiris4 <- anim_prep(data = osiris,
                     id = ID,
                     values = sales,
                     time = year,
                     ncat = 5L,
                     group_scaling = country,
                     group = japan,
                     scaling = "absolute")

osiris_p <- wallaby_plot(data = osiris1,
                         group_palette = RColorBrewer::brewer.pal(8, "Paired"),
                         shade_palette = RColorBrewer::brewer.pal(9, "Set1"),
                         subset = "top",
                         rendering = "gganimate",
                         x_lab = c("start", "end"))

osiris_anim <- anim_animate(osiris_p)

gganimate::animate(osiris_anim, nframes = 89)

plotly::ggplotly(osiris_anim)

osiris_us <- osiris %>%
  mutate(usa = ifelse(country == "US", "usa", "others")) %>%
  mutate(lsales = log(sales)) |>
  filter(between(year, 2006, 2008))

osiris5 <- anim_prep(data = osiris_us,
                     id = ID,
                     values = lsales,
                     time = year,
                     ncat = 5L,
                     group = usa,
                     #scaling = "absolute",
                     group_scaling = country)

osiris_p <- wallaby_plot(data = osiris5,
                         group_palette = RColorBrewer::brewer.pal(8, "Dark2"),
                         shade_palette = RColorBrewer::brewer.pal(9, "Set1"),
                         subset = "bottom",
                         size = 5)

osiris_anim <- anim_animate(osiris_p)

gganimate::animate(osiris_anim)

# US v China (bc US has few observations in previous view)
osiris_us_china <- osiris %>%
  filter(country %in% c("US", "CN")) %>%
  mutate(usa = ifelse(country == "US", "usa", "china")) %>%
  mutate(lsales = log(sales))

osiris5 <- anim_prep(data = osiris_us_china,
                     id = ID,
                     values = lsales,
                     time = year,
                     ncat = 5L,
                     group = usa,
                     #scaling = "absolute",
                     group_scaling = country)

osiris5 %>% count(qtile)

osiris_p <- wallaby_plot(data = osiris5,
                         group_palette = RColorBrewer::brewer.pal(8, "Dark2"),
                         shade_palette = c("#737373", "#969696", "#BDBDBD","#D9D9D9","#D9D9D9","#D9D9D9"),
                         subset = "top",
                         total_point = 1000)

osiris_anim <- anim_animate(osiris_p)

gganimate::animate(osiris_anim)

# aeles -------------------------------------------------------------------

order <- c("liberal", "greens", "not vote", "other", "national", "labor")

object <- anim_prep_cat(data = aeles,
                        id = id,
                        values = party,
                        time = year,
                        group = gender,
                        order = order,
                        label = order)

# subset can be either "top", "bottom" or values in the data
# many_one not working yet

p <- wallaby_plot(data = object,
                  group_palette = RColorBrewer::brewer.pal(9, "Set1"),
                  shade_palette = RColorBrewer::brewer.pal(9, "Set1"),
                  rendering = "gganimate",
                  subset = "top",
                  relation = "one_many",
                  height = 1,
                  size = 3,
                  width = 100,
                  total_point = 1000)

p2 <- anim_animate(p)

gganimate::animate(p2, nframes = 139)

plotly::ggplotly(p2)


