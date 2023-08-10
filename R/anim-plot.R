#'@importFrom ggplot2 ggplot geom_point geom_hline annotate theme
#'@export

anim_plot <- function(data,
                      id = NULL,
                      time = NULL,
                      color = NULL,
                      label = NULL,
                      palette = RColorBrewer::brewer.pal(9, "Set1"),
                      rendering = "gganimate") {

  hline <- unique(data$qtile)

  qx <- enquo(time)

  x <- data[, as_label(qx)]

  y <- sort(unique(data$qtile), decreasing = TRUE)

  col_val <- palette

  # use for annotate function

  stopifnot("The length of label is not the same as the length of y-axis values" =
              length(label) >= length(y))

  if (is.null(label)) {
    label <- as.character(sort(unique(data$qtile), decreasing = TRUE))
  }

  if (length(label) >= length(y)) {
    label <- label[1:length(y)]
  }

# build plot --------------------------------------------------------------

  anim <- data |>
    ggplot() +
    geom_jitter(aes(x = {{ time }}, y = qtile, group = {{ id }}, color = {{ color }}), height = 0.1) +
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
         plot.margin = margin(1, 1, 1, 3, "cm"),
         legend.position = "bottom",
         legend.title = element_blank()) +
    scale_colour_manual(values = col_val) +
    annotate("text", x = min(x) - 1.5, y = y, label = label)
}
