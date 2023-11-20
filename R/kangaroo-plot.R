#' Turn the data into a ggplot object for the animate function
#'
#' This function takes in the data which has been prepared by the [anim_prep()]
#' or [anim_prep_cat()] and return the ggplot object. The user can still modify
#' the plot the same as normal.
#'
#' @param data The categorized data.
#' @param group_palette The vector of the palette used by the function to supply
#' the color to each group.
#' @param shade_palette The vector of the palette used by the function to supply
#' the color to the shaded area.
#' @param rendering The choice of method used to create and display the plot,
#' either gganimate or plotly.
#' @param time_dependent Logical. Should the visualization be time-dependent?
#' The default is FALSE
#' @param ... Additional arguments for customization, see datails for more
#' information.
#'
#' @return Return a ggplot object
#'
#' @details
#' This function takes categorized data and generates a ggplot object.
#' The kangaroo plot is the plot that shows the movement between group over
#' time. The point position and point size in the shaded area can be controlled
#' using additional arguments such as height, width, and size. For the shading
#' area, the alpha argument can be used.
#'
#' @examples
#' example <- anim_prep(data = dbl_change, id = id, values = values,
#' time = time, group = gp)
#'
#' kangaroo_plot(example)
#'
#' @importFrom ggplot2 element_blank
#'
#' @export

kangaroo_plot <- function(data,
                          group_palette = NULL,
                          shade_palette = NULL,
                          rendering = "gganimate",
                          time_dependent = FALSE,
                          ...) {


  # check -------------------------------------------------------------------

  rendering_choice <- c("gganimate", "plotly")

  stopifnot("The rendering argument can only by either gganimate or plotly" =
              rendering %in% rendering_choice)


  # group color palette -----------------------------------------------------

  if (is.null(group_palette)) {
    group_palette <- RColorBrewer::brewer.pal(9, "Set1")
  }

  if (!is.null(group_palette)) {
    group_palette <- group_palette
  }


  # shade color palette -----------------------------------------------------

  if (is.null(shade_palette)) {
    shade_palette <- c("#bbbbbb",
                       "#aaaaaa",
                       "#999999",
                       "#888888",
                       "#777777",
                       "#666666",
                       "#555555",
                       "#444444",
                       "#333333")
  }

  if (!is.null(shade_palette)) {
    shade_palette <- shade_palette
  }


  # ... arguments -----------------------------------------------------------

  args <- list(...)

  # height settings
  height <- 0.6

  if (!is.null(args[["height"]])) {
    height <- args[["height"]]
  }

  # width settings
  width <- 50L

  if (!is.null(args[["width"]])) {
    width <- as.integer(args[["width"]])
  }

  # alpha settings for paths shading
  alpha <- 0.1

  if (!is.null(args[["alpha"]])) {
    alpha <- args[["alpha"]]
  }

  # size settings for point
  size <- 2

  if (!is.null(args[["size"]])) {
    size <- args[["size"]]
  }

  args_accepted <- c("height", "width", "alpha", "size")

  if (any(!(names(args) %in% args_accepted))) {
    warning(paste0("The following arguments are not supported: ",
                   paste0(names(args)[!(names(args) %in% args_accepted)], collapse = ", ")))
  }


  # format data -------------------------------------------------------------

  object <- kangaroo_data(data = data,
                          time_dependent = time_dependent,
                          height = height,
                          width = width)


  # variable main aes() -----------------------------------------------------

  # gganimate rendering
  if (rendering == "gganimate") {

    if ("group" %in% colnames(object[["data"]])) {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        group = quote(id),
        color = quote(group)
      )
    }

    if (!("group" %in% colnames(object[["data"]]))) {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        group = quote(id)
      )
    }
  }

  # plotly rendering
  if (rendering == "plotly") {

    if ("group" %in% colnames(object[["data"]])) {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        color = quote(group),
        ids = quote(id),
        frame = quote(frame)
      )
    }

    if (!("group" %in% colnames(object[["data"]]))) {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        ids = quote(id),
        frame = quote(frame)
      )
    }
  }


  # draw plot ---------------------------------------------------------------

  # the data point
  kangaroo <- ggplot2::ggplot() +
    ggplot2::geom_point(data = object[["data"]],
                        mapping = ggplot2::aes(!!!aes_list),
                        size = size
    ) |>
    suppressWarnings()

  # The shaded area + label
  australia <- kangaroo +
    ggplot2::geom_rect(data = object[["shade_data"]],
                       ggplot2::aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax,
                                    group = id),
                       alpha = alpha) +
    ggplot2::geom_text(data = object[["label_data"]],
                       ggplot2::aes(x = x,
                                    y = y,
                                    label = label))

  # plot settings
  anim <- australia +
    ggplot2::scale_x_continuous(breaks = object[["xbreaks"]]) +
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
    ggplot2::scale_color_manual(values = group_palette) +
    ggplot2::scale_fill_manual(values = shade_palette)

  message("You can now used the animbook::anim_animate() function to transformed
          it to an animated object")

  class(anim) <- c("ggplot", "gg", "animated", "kangaroo", rendering)

  return(anim)

}
