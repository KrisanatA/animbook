#' @importFrom ggplot2 ggplot geom_point geom_hline annotate theme

anim_plot <- function(data, id = NULL, x_axis = NULL, color = NULL, palette = RColorBrewer::brewer.pal(9, "Set1")) {

  hline <- unique(data$pos)

  qx <- enquo(x_axis)

  x <- data[, as_label(qx)]

  col_val <- palette

# build plot --------------------------------------------------------------

  anim <- data |>
    ggplot() +
    geom_jitter(aes(x = {{ x_axis }}, y = pos, group = {{ id }}, color = {{ color }}), height = 0.1) +
    geom_hline(yintercept = hline, linewidth = 6, alpha = 0.1) +
    scale_x_continuous(breaks = seq(min(x), max(x), 1)) +
    coord_cartesian(xlim = c(min(x), max(x)),
                    clip = 'off') +
    theme(aspect.ratio = 2/3,
         panel.background = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.line.y = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.x = element_blank(),
         plot.margin = margin(1, 1, 1, 1, "cm"),
         legend.position = "bottom",
         legend.title = element_blank()) +
    scale_colour_manual(values = col_val)

  #   ggplot2::annotate("text", x = 2007.5, y = 5, label = "Top 20%") +
  #   ggplot2::annotate("text", x = 2007.5, y = 4, label = "21 - 40") +
  #   ggplot2::annotate("text", x = 2007.5, y = 3, label = "41 - 60") +
  #   ggplot2::annotate("text", x = 2007.5, y = 2, label = "61 - 80") +
  #   ggplot2::annotate("text", x = 2007.5, y = 1, label = "81 - 100") +
  #   ggplot2::annotate("text", x = 2007.5, y = 0, label = "Not listed") +
}
