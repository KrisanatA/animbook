#' @import ggplot2

anim_plot <- function(df, x, y, id) {
  data <- anim_data(df, {{id}}, {{y}})

  anim <- data |>
    ggplot2::ggplot() +
    ggplot2::geom_point(aes(x = {{x}}, y = y, group = {{id}}, color = japan)) +
    ggplot2::geom_hline(yintercept = 0:5, linewidth = 5.6, alpha = 0.1) +
    ggplot2::annotate("text", x = 2007.5, y = 5, label = "Top 20%") +
    ggplot2::annotate("text", x = 2007.5, y = 4, label = "21 - 40") +
    ggplot2::annotate("text", x = 2007.5, y = 3, label = "41 - 60") +
    ggplot2::annotate("text", x = 2007.5, y = 2, label = "61 - 80") +
    ggplot2::annotate("text", x = 2007.5, y = 1, label = "81 - 100") +
    ggplot2::annotate("text", x = 2007.5, y = 0, label = "Not listed") +
    ggplot2::scale_colour_manual(name = "",
                        breaks = c("Yes", "No"),
                        labels = c("From Japan", "Not from Japan"),
                        values = c("red", "blue")) +
    ggplot2::scale_x_continuous(breaks = seq(2009, 2018, 1)) +
    ggplot2::coord_cartesian(xlim = c(2009, 2018),
                    clip = 'off') +
    ggplot2::theme(aspect.ratio = 2/3,
          panel.background = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(1, 1, 1, 4, "cm"),
          legend.position = "bottom")
}
