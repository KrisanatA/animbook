#' Turn the data into a facetted plot
#'
#' This function takes in the data which has been prepared by either [anim_prep()] or [anim_prep_cat()] and
#' return the ggplot object. The user can still modify the plot the same as normal using the ggplot2 function.
#'
#' @param data The animbook object returned from the prep function.
#' @param group_palette The vector of the palette used by the function to supply the color to each group.
#' @param ... Additional arguments for customization, see details for more information.
#'
#' @return Return a ggplot object
#'
#' @details
#' This function takes prepared data and generates a ggplot object.
#' The funnel web plot is the plot that shows the line facetted plot showing the pattern between
#' time period.
#' The line jitter can be controlled using additional arguments such as height and width
#' to control the appearance. For the shading area, the alpha argument can be used.
#'
#' @examples
#' animbook <- anim_prep(data = osiris, id = ID, values = sales, time = year, group = japan)
#'
#' funnel_web_plot(animbook)
#'
#' @importFrom ggplot2 element_blank
#'
#' @export

funnel_web_plot <- function(data,
                            group_palette = NULL,
                            ...) {

  # group color palette -----------------------------------------------------

  if (is.null(group_palette)) {
    group_palette <- RColorBrewer::brewer.pal(9, "Set1")
  }

  if (!is.null(group_palette)) {
    group_palette <- group_palette
  }


# ... arguments -----------------------------------------------------------

  args <- list(...)

  # height settings for geom_jitter
  height <- 0.2

  if (!is.null(args[["height"]])) {
    height <- args[["height"]]
  }

  # width settings for geom_jitter
  width <- 0

  if (!is.null(args[["width"]])) {
    width <- args[["width"]]
  }

  # alpha settings for paths shading
  alpha <- 0.1

  if (!is.null(args[["alpha"]])) {
    alpha <- args[["alpha"]]
  }

  args_accepted <- c("height", "width", "alpha")

  if (any(!(names(args) %in% args_accepted))) {
    warning(paste0("The following arguments are not supported: ",
                   paste0(names(args)[!(names(args) %in% args_accepted)], collapse = ", ")))
  }


# format data -------------------------------------------------------------

  object <- funnel_web_spider_data(data)


# variable main aes() -----------------------------------------------------

  if ("group" %in% colnames(object[["data"]])) {
    aes_list <- list(
      x = quote(time),
      y = quote(qtile),
      group = quote(id),
      color = quote(group)
    )
  }

  else {
    aes_list <- list(
      x = quote(time),
      y = quote(qtile),
      group = quote(id)
    )
  }



# draw plot ---------------------------------------------------------------

  # data point
  australia <- ggplot2::ggplot() +
    ggplot2::geom_line(data = object[["data"]],
                       mapping = ggplot2::aes(!!!aes_list),
                       position = ggplot2::position_jitter(height = height,
                                                           width = width)) +
    ggplot2::facet_wrap(~facet, scales = "free_x")

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
    ggplot2::scale_colour_manual(values = group_palette)

  return(anim)
}
