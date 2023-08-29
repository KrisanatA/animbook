#'Line Plot with Facets
#'
#'This function creates a line plot with facets based on time periods and grouping variable.
#'
#'@param data A data frame containing the input data.
#'@param palette The vector of palette used by the function to supplied the color for each group.
#'
#'@return Return a ggplot object
#'
#'@examples
#'data <- anim_prep(data = osiris, id = firmID, values = sales, time = year)
#'
#'facet_plot(data)
#'
#'@importFrom ggplot2 element_blank
#'
#'@export

facet_plot <- function(data,
                       palette = RColorBrewer::brewer.pal(9, "Set1")) {

  col_val <- palette

  start <- data[["data"]] |>
    dplyr::filter(time > min(time)) |>
    dplyr::group_by(id) |>
    dplyr::mutate(facet = dplyr::row_number())

  end <- data[["data"]] |>
    dplyr::filter(time < max(time)) |>
    dplyr::group_by(id) |>
    dplyr::mutate(facet = dplyr::row_number())

  full <- rbind(start, end) |>
    dplyr::arrange(id, time)


  anim <- ggplot2::ggplot() +
    ggplot2::geom_line(data = full, ggplot2::aes(x = time, y = qtile, group = id, color = color),
                       position = ggplot2::position_jitter(height = 0.2, width = 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_x_continuous(breaks = scale1[["settings"]]$breaks) +
    ggplot2::theme(panel.background = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.line.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   legend.position = "bottom",
                   legend.title = element_blank()) +
    ggplot2::scale_colour_manual(values = col_val) +
    ggplot2::facet_wrap(~facet, scales = "free_x")

  return(anim)
}
