#'@export

kangaroo_plot <- function(object,
                          palette = RColorBrewer::brewer.pal(9, "Set1"),
                          rendering = "ggplot",
                          ...) {

  col_val <- palette

  rendering_choice <- c("ggplot", "plotly")

  stopifnot("Please use the anim_prep function to converted data into animbook class object" =
              class(object) == "animbook",
            "The rendering argument can only be either ggplot or plotly" =
              rendering %in% rendering_choice)


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

  object <- kangaroo_data(object)


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

  # the data point
  australia <- ggplot2::ggplot() +
    ggplot2::geom_jitter(data = object[["data"]],
                         mapping = ggplot2::aes(!!!aes_list),
                         height = height, width = width) |>
    suppressWarnings()

  # the shaded area + label
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

  # plot settings
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












#'Kangaroo plot data
#'
#'This function performs data manipulation and formatting tasks
#'from the original object with additional data components for labeling and shading.
#'
#'@param object An animbook object
#'
#'@return A modified animbook object with addition data components
#'
#'@details The function takes the animbook object and create a new label data and
#'shading data used for the plot then appends them to the original object.
#'
#'@keywords internal

kangaroo_data <- function(object) {

  data <- object[["data"]]

  x <- unique(data$time)

  y <- sort(unique(data$qtile), decreasing = TRUE)

  gap <- object[["settings"]]$gap

  unique_qtiles <- unique(data$qtile)

# Create label data -------------------------------------------------------

  label_data <- tibble::tibble(
    x = min(x) - (2 * gap),
    y = y,
    label = object[["settings"]]$label
  )


# Create shading data -----------------------------------------------------

  shade_data <- tibble::tibble(
    id = y,
    xmin = rep(min(x) - gap, length(y)),
    xmax = rep(max(x) + gap, length(y)),
    ymin = y - 0.25,
    ymax = y + 0.25
  )


  kangaroo <- append(object, list(label_data, shade_data))

  names(kangaroo) <- c("data", "settings", "label_data", "shade_data")

  return(kangaroo)

}

