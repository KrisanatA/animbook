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

  rendering_choice <- c("plotly", "gganimate")

  stopifnot("rendering argument can only be either gganimate or plotly" =
              rendering %in% rendering_choice)

  # use for annotate function

  if (is.null(label)) {
    label <- as.character(sort(unique(data$qtile), decreasing = TRUE))
  }

  if (length(label) >= length(y)) {
    label <- label[1:length(y)]
  }

  stopifnot("The length of label is not the same as the length of y-axis values" =
              length(label) >= length(y))


  gap <- 0.1 * (nrow(unique(x)) - 1)

  rect_data <- data.frame(
    id = y,
    xmin = rep(min(x) - gap, length(y)),
    xmax = rep(max(x) + gap, length(y)),
    ymin = y - 0.25,
    ymax = y + 0.25
  )

  label_data <- data.frame(
    x = min(x) - (2 * gap),
    y = y,
    label = label
  )



# build plot --------------------------------------------------------------

  if (rendering == "gganimate") {
    jitter <- ggplot() +
      geom_jitter(data = data, aes(x = {{ time }}, y = qtile, group = {{ id }}, color = {{ color }}), height = 0.2, width = 0)
  }

  if (rendering == "plotly") {
    jitter <- ggplot() +
      geom_jitter(data = data, aes(x = {{ time }}, y = qtile, color = {{color}}, ids = {{ id }}, frame = frame),
                  height = 0.2, width = gap) |>
      suppressWarnings()
  }

  anim <- jitter +
    geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group =  id), alpha = 0.1) +
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
