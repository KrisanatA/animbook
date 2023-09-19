library(animbook)

# osiris ------------------------------------------------------------------

label <- c("Top20", "20-40", "40-60", "60-80", "80-100", "Not listed")

osiris1 <- anim_prep(data = osiris, id = ID, values = sales, time = year, label = label,
                     ngroup = 5L, color = japan)

osiris2 <- anim_prep(data = osiris, id = ID, values = sales, time = year, label = label,
                     ngroup = 5L, group_scaling = country, color = japan)

osiris3 <- anim_prep(data = osiris, id = ID, values = sales, time = year,
                     ngroup = 5L, color = japan, scaling = "absolute")

osiris4 <- anim_prep(data = osiris, id = ID, values = sales, time = year,
                     ngroup = 5L, group_scaling = country, color = japan, scaling = "absolute")




# aeles -------------------------------------------------------------------

order <- c("liberal", "not vote", "greens", "other", "national", "labor")

object <- anim_prep_cat(data = aeles, id = id, values = party, time = year,
                        order = NULL, color = gender, time_dependent = FALSE,
                        runif_min = 1, runif_max = 100)

# subset can be either "top", "bottom" or values in the data
# many_one not working yet

p <- wallaby_plot(object = object, palette = RColorBrewer::brewer.pal(9, "Set1"),
                  rendering = "ggplot", subset = "bottom", relation = "many_one")


p <- kangaroo_plot(object)

p2 <- anim_animate(p)

