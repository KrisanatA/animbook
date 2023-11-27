#' Turn the data into a subset plot for animate function
#'
#' This function takes in the data which has been prepared by the [anim_prep()]
#' or [anim_prep_cat()] and return the ggplot object. The user can still modify
#' the plot as usual.
#'
#' @param data The categorized data.
#' @param group_palette The vector of the palette used by the function to supply
#' the color of each group.
#' @param shade_palette The vector of the palette used by the function to supply
#' the color of each shaded area.
#' @param rendering The choice of method used to create and display the plot,
#' either gganimate or plotly.
#' @param time_dependent Logical. Should the visualization be time-dependent?
#' The default is FALSE.
#' @param subset A character string specifying the variable used for subsetting
#' the data. The "top" and "bottom" strings can also be used in this argument.
#' @param relation The choice of relationship for the values to display on the
#' plot, either "one_many" or "many_one."
#' @param total_point The number of points the users want for the wallaby plot.
#' The default is NULL, where the number of the point is equal to the original number
#' of points.
#' @param x_lab The label for the x-axis.
#' @param ... Additional arguments for customization. See details for more
#' information.
#'
#' @return Return a ggplot object.
#'
#' @details
#' This function takes categorized data and generates a ggplot object.
#' The wallaby plot is the plot that shows the movement of the subset data
#' between the start and end of the observable period. The point position and point
#' size in the shaded area can be controlled using additional arguments such as
#' height, width, and size. For the shading area, the alpha argument can be used.
#'
#' @examples
#' animbook <- anim_prep(data = osiris, id = ID, values = sales, time = year, group = japan)
#'
#' wallaby_plot(animbook)
#'
#' @importFrom ggplot2 element_blank
#'
#' @export

wallaby_plot <- function(data,
                        group_palette = NULL,
                        shade_palette = NULL,
                        rendering = "gganimate",
                        time_dependent = FALSE,
                        subset = "top",
                        relation = "one_many",
                        total_point = NULL,
                        x_lab = NULL,
                        ...) {


  # check -------------------------------------------------------------------

  rendering_choice <- c("gganimate", "plotly")

  stopifnot("The rendering argument can only be either gganimate or plotly" =
              rendering %in% rendering_choice)


  # x label -----------------------------------------------------------------

  if (is.null(x_lab)) {
    x_lab <- c("", "")
  }

  if (length(x_lab) > 2) {
    x_lab <- x_lab[1:2]

    warning("The length of the x_lab provided is greater than 2, the function
            only takes the first two elements")
  }

  if (length(x_lab) == 2) {
    x_lab <- x_lab
  }

  if (length(x_lab) < 2) {
    x_lab <- c("", "")

    warning("The length of the x_lab provided is less than 2")
  }


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

  object <- wallaby_data(data = data,
                        subset = subset,
                        relation = relation,
                        time_dependent = time_dependent,
                        height = height,
                        width = width,
                        total_point = total_point)


  # variable main aes() -----------------------------------------------------

  if (rendering == "gganimate") {

    if ("group" %in% colnames(object[["data"]])) {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        group = quote(id),
        color = quote(group)
      )
    }

    else {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        group = quote(id)
      )
    }
  }

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

    else {
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
  australia <- ggplot2::ggplot() +
    ggplot2::geom_point(data = object[["data"]],
                        mapping = ggplot2::aes(!!!aes_list),
                        size = size
    ) |>
    suppressWarnings()

  # the shaded area + label
  australia <- australia +
    ggplot2::geom_polygon(data = object[["shade_data"]],
                          ggplot2::aes(x = x,
                                       y = y,
                                       group = id,
                                       fill = as.factor(id)),
                          alpha = alpha) +
    ggplot2::geom_text(data = object[["left_data"]],
                       ggplot2::aes(x = x,
                                    y = y,
                                    label = label)) +
    ggplot2::geom_text(data = object[["right_data"]],
                       ggplot2::aes(x = x,
                                    y = y,
                                    label = label)) |>
    suppressWarnings()

  # plot settings
  anim <- australia +
    ggplot2::scale_x_continuous(breaks = c(0, 1),
                                label = x_lab) +
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
    ggplot2::guides(fill = "none") +
    ggplot2::scale_fill_manual(values = shade_palette) +
    ggplot2::scale_colour_manual(values = group_palette)

  message("You can now use the animbook::anim_animate() function to
          transform it into an animated object")

  class(anim) <- c("ggplot", "gg", "animated", "wallaby", rendering)

  return(anim)

}
