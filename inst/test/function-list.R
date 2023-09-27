library(animbook)

# osiris ------------------------------------------------------------------

label <- c("Top20", "20-40", "40-60", "60-80", "80-100", "Not listed")

osiris1 <- anim_prep(data = osiris,
                     id = ID,
                     values = sales,
                     time = year,
                     label = label,
                     ngroup = 5L,
                     color = japan)

osiris2 <- anim_prep(data = osiris, id = ID, values = sales, time = year, label = label,
                     ngroup = 5L, group_scaling = country, color = japan)

osiris3 <- anim_prep(data = osiris,
                     id = ID,
                     values = sales,
                     time = year,
                     ngroup = 5L,
                     color = japan,
                     scaling = "absolute",
                     time_dependent = FALSE)

osiris4 <- anim_prep(data = osiris, id = ID, values = sales, time = year,
                     ngroup = 5L, group_scaling = country, color = japan, scaling = "absolute")

osiris_p <- wallaby_plot(object = osiris3,
                         palette = RColorBrewer::brewer.pal(9, "Set1"),
                         subset = "top")

osiris_anim <- anim_animate(osiris_p)

gganimate::animate(osiris_anim)

plotly::ggplotly(osiris_anim)

osiris_us <- osiris %>%
  mutate(usa = ifelse(country == "US", "usa", "others")) %>%
  mutate(lsales = log(sales))

osiris5 <- anim_prep(data = osiris_us,
                     id = ID,
                     values = lsales,
                     time = year,
                     ngroup = 5L,
                     color = usa,
                     scaling = "absolute",
                     group_scaling = country,
                     time_dependent = FALSE)

osiris_p <- wallaby_plot(object = osiris5,
                         palette = RColorBrewer::brewer.pal(8, "Paired"),
                         subset = "top")

osiris_anim <- anim_animate(osiris_p)

gganimate::animate(osiris_anim)

# aeles -------------------------------------------------------------------

order <- c("liberal", "not vote", "greens", "other", "national", "labor")

object <- anim_prep_cat(data = aeles, id = id, values = party, time = year,
                        order = NULL, color = gender, time_dependent = FALSE)

# subset can be either "top", "bottom" or values in the data
# many_one not working yet

p <- wallaby_plot(object = object, palette = RColorBrewer::brewer.pal(9, "Set1"),
                  rendering = "ggplot", subset = "greens", relation = "one_many",
                  height = 1)

p2 <- anim_animate(p)

gganimate::animate(p2)

plotly::ggplotly(p2)


