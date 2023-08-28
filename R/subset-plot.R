subset_plot <- function(data,
                        subset,
                        palette = RColorBrewer::brewer.pal(9, "Set1"),
                        rendering = "gganimate") {

  col_val <- palette

  rendering_choice <- c("plotly", "gganimate")

  stopifnot("rendering argument can only be either gganimate or plotly" =
              rendering %in% rendering_choice)

}




subset_plot <- function(data,
                        palette = RColorBrewer::brewer.pal(9, "Set1"),
                        rendering = "gganimate") {

  qx <- enquo(time)

  qid <- enquo(id)

  qtime <- enquo(time)

  x <- data[, as_label(qx)]

  y <- sort(unique(data$qtile), decreasing = TRUE)

  col_val <- palette

  rendering_choice <- c("plotly", "gganimate")

  stopifnot("rendering argument can only be either gganimate or plotly" =
              rendering %in% rendering_choice)

  # assign label to y-axis

  if (is.null(label)) {
    label <- as.character(y)
  }

  if (length(label) >= length(y)) {
    label <- label[1:length(y)]
  }

  if (length(label) < length(y)) {
    label <- as.character(y)
    # will need to add warning
  }

  subset <- data |>
    filter(!!qtime == min(x),
           qtile == max(unique(data$qtile))) |>
    pull(!!qid)

  data <- data |>
    filter(!!qid %in% subset) |>
    mutate(x_axis = case_when(
      !!qtime == min(x) ~ 0,
      !!qtime == max(x) ~ 1
    ))

  gap <- 0.1 * (length(unique(data$x_axis)) - 1)

  left <- data.frame(
    id = y,
    x = min(data$x_axis),
    y = max(y)
  )

  right <- data.frame(
    id = y,
    x = max(data$x_axis),
    y = y
  )

  line_shade <- rbind(left, right)

  label_right <- data.frame(
    x = max(data$x_axis) + (3 * gap),
    y = y,
    label = label
  )

  label_left <- data.frame(
    x = min(data$x_axis) - (3 * gap),
    y = max(y),
    label = label[1]
  )


# build plot --------------------------------------------------------------

  if (rendering == "gganimate") {
    jitter <- ggplot() +
      geom_jitter(data = data, aes(x = x_axis, y = qtile, group = {{ id }},
                                   color = {{ color }}),
                  height = 0.2, width = 0.2)
  }

  if (rendering == "plotly") {
    jitter <- ggplot() +
      geom_jitter(data = data, aes(x = x_axis, y = qtile, color = {{ color }},
                                   ids = {{ id }}, frame = frame),
                  height = 0.2, width = gap) |>
      suppressWarnings()
  }

  anim <- jitter +
    # need to figure out the shade
    # geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin,
    #                                 ymax = ymax, group = id),
    #           alpha = 0.1) +
    geom_text(data = label_right, aes(x = x, y = y, label = label)) +
    geom_text(data = label_left, aes(x = x, y = y, label = label)) +
    scale_x_continuous(breaks = c(0, 1)) +
    coord_cartesian(clip = "off") +
    theme(panel.background = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    scale_colour_manual(values = col_val)


  return(anim)

}












