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

  if (is.null(label)) {
    label <- as.character(sort(unique(data$qtile), decreasing = TRUE))
  }

  if (length(label) >= length(y)) {
    label <- label[1:length(y)]
  }

  stopifnot("The length of label is not the same as the length of y-axis values" =
              length(label) >= length(y))

  rect_data <- data.frame(
    id = y,
    xmin = rep(min(x), length(y)),
    xmax = rep(max(x), length(y)),
    ymin = y - 0.15,
    ymax = y + 0.15
  )

  gap <- 0.1 * (nrow(unique(x)) - 1)

  label_data <- data.frame(
    x = min(x) - gap,
    y = y,
    label = label
  )



# build plot --------------------------------------------------------------

  anim <- ggplot() +
    geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group =  id), alpha = 0.1) +
    geom_jitter(data = data, aes(x = {{ time }}, y = qtile, group = {{ id }}, color = {{ color }}), height = 0.1, width = 0) +
    geom_text(data = label_data, aes(x = x, y = y, label = label)) +
    scale_x_continuous(breaks = seq(min(x), max(x), 1)) +
    coord_cartesian(clip = "off") +
    theme(panel.background = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.line.y = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.x = element_blank(),
         legend.position = "bottom",
         legend.title = element_blank()) +
    scale_colour_manual(values = col_val)

  return(anim)
}
