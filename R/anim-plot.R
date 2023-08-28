#'Turn the data into ggplot object for animate function
#'
#'This function take in the data which has been prepared by the [prep_anim()] and
#'return the ggplot object. The user can still modified the plot the same as normal.
#'
#'@param data The data frame that has been prepared by the [prep_anim()]
#'@param palette The vector of palette used by the function to supplied the color for each group.
#'@param rendering The choice of method used to create and display the plot, either gganimate or
#'plotly.
#'
#'@return Return a ggplot object
#'
#'@examples
#'data <- anim_prep(data = toy_dbl, id = id, values = values, time = year, label = check, color = color)
#'
#'anim_plot(data)
#'
#'@export

anim_plot <- function(data,
                      palette = RColorBrewer::brewer.pal(9, "Set1"),
                      rendering = "gganimate") {

  col_val <- palette

  rendering_choice <- c("plotly", "gganimate")

  stopifnot("rendering argument can only be either gganimate or plotly" =
              rendering %in% rendering_choice)

  x <- data[["settings"]]$breaks

  gap <- data[["settings"]]$gap

  label_data <- data.frame(
    x = min(x) - (2 * gap),
    y = sort(unique(data[["data"]]$qtile), decreasing = TRUE),
    label = data[["settings"]]$label
  )


  if (rendering == "gganimate") {
    jitter <- ggplot2::ggplot() +
      ggplot2::geom_jitter(data = data[["data"]], ggplot2::aes(x = time, y = qtile, group = id,
                                                               color = color), height = 0.2, width = 0)
  }

  if (rendering == "plotly") {
    jitter <- ggplot2::ggplot() +
      ggplot2::geom_jitter(data = data[["data"]], ggplot2::aes(x = time, y = qtile, color = color,
                                                               ids = id , frame = frame),
                  height = 0.2, width = gap) |>
      suppressWarnings()
  }

  anim <- jitter +
    ggplot2::geom_rect(data = data[["rect_data"]], ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group =  id), alpha = 0.1) +
    ggplot2::geom_text(data = label_data, aes(x = x, y = y, label = label)) +
    ggplot2::scale_x_continuous(breaks = seq(min(x), max(x), 1)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(panel.background = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    ggplot2::scale_colour_manual(values = col_val)

  return(anim)

}

