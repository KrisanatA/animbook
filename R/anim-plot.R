#'Turn the data into ggplot object for animate function
#'
#'This function take in the data which has been prepared by the [anim_prep()] and
#'return the ggplot object. The user can still modified the plot the same as normal.
#'
#'@param object The data frame that has been prepared by the [anim_prep()]
#'@param plot The type of plot to generate. Choose from "kangaroo," "wallaby," or
#'"funnel_web_spider."
#'@param palette The vector of palette used by the function to supplied the color for each group.
#'@param rendering The choice of method used to create and display the plot, either gganimate or
#'plotly.
#'@param ... Additional arguments for customization.
#'
#'@return Return a ggplot object
#'
#'@details
#'This function takes prepared data and generates a ggplot object.
#'Users can specify the type of plot to create ("kangaroo," "wallaby," or
#'"funnel_web_spider") and customize the appearance of the plot using additional
#'arguments.
#'
#'@examples
#'animbook <- anim_prep(data = osiris, id = ID, values = sales, time = year, color = japan)
#'
#'anim_plot(animbook)
#'
#'@importFrom ggplot2 element_blank
#'
#'@export

anim_plot <- function(object,
                      plot = "kangaroo",
                      palette = RColorBrewer::brewer.pal(9, "Set1"),
                      rendering = "ggplot",
                      ...) {

  col_val <- palette

  plot_choice <- c("kangaroo", "wallaby", "funnel_web_spider")

  rendering_choice <- c("ggplot", "plotly")

  if (plot == "funnel_web_spider") {
    funnel <- ifelse(rendering == "ggplot", TRUE, FALSE)
  }

  else {
    funnel <- TRUE
  }

  stopifnot("Please use the anim_prep function to converted data into animbook class object" =
              class(object) == "animbook",
            "Please check all the plot supported" =
              plot %in% plot_choice,
            "Funnel web spider does not support plotly" =
              funnel == TRUE
            )


# ... arguments -----------------------------------------------------------

  args <- list(...)

  # subset for wallaby plot
  subset <- "top"

  if (!is.null(args[["subset"]])) {
    subset <- args[["subset"]]
  }

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


# reformat data -----------------------------------------------------------

  if (plot == "kangaroo") {
    object <- kangaroo_data(object)
  }

  if (plot == "wallaby") {
    object <- wallaby_data(object)
  }

  if (plot == "funnel_web_spider") {
    object <- funnel_web_spider_data(object)
  }


# variable main aes() -----------------------------------------------------

  if (rendering == "ggplot") {

    if ("color" %in% colnames(object[["data"]])) {
      aes_list <- list(
        x = quote(time),
        y = quote(qtile),
        group = quote(id),
        color = quote(color)
      )
    }

    else {
      aes_list <- list(
        x = quote(time),
        y = quote(qtile),
        group = quote(id)
      )
    }
  }

  if (rendering == "plotly") {

    if ("color" %in% colnames(object[["data"]])) {
      aes_list <- list(
        x = quote(time),
        y = quote(qtile),
        color = quote(color),
        ids = quote(id),
        frame = quote(frame)
      )
    }

    else {
      aes_list <- list(
        x = quote(time),
        y = quote(qtile),
        ids = quote(id),
        frame = quote(frame)
      )
    }
  }


# draw plot ---------------------------------------------------------------

  if (plot == "kangaroo") {
    australia <- ggplot2::ggplot() +
      ggplot2::geom_jitter(data = object[["data"]],
                           mapping = ggplot2::aes(!!!aes_list),
                           height = height, width = width) |>
      suppressWarnings()

    australia <- australia +
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
  }

  if (plot == "wallaby") {
    australia <- ggplot2::ggplot() +
      ggplot2::geom_jitter(data = object[["data"]],
                           mapping = ggplot2::aes(!!!aes_list),
                           height = height, width = width) |>
      suppressWarnings()

    australia <- australia +
      ggplot2::geom_polygon(data = object[["shade_data"]],
                            ggplot2::aes(x = x,
                                         y = y,
                                         group = id),
                            alpha = alpha) +
      ggplot2::geom_text(data = object[["label_data"]]$right,
                         ggplot2::aes(x = x,
                                      y = y,
                                      label = label)) +
      ggplot2::geom_text(data = object[["label_data"]]$left,
                         ggplot2::aes(x = x,
                                      y = y,
                                      label = label)) |>
      suppressWarnings()
  }

  if (plot == "funnel_web_spider") {
    australia <- ggplot2::ggplot() +
      ggplot2::geom_line(data = object[["data"]],
                         mapping = ggplot2::aes(!!!aes_list),
                         position = ggplot2::position_jitter(height = height,
                                                             width = width)) +
      ggplot2::facet_wrap(~facet, scales = "free_x")
  }

  anim <- australia +
    ggplot2::scale_x_continuous(breaks = object[["settings"]]$xbreaks) +
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

