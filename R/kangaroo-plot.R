#' Turn the data into a ggplot object for the animate function
#'
#' This function takes in the data which has been prepared by the [anim_prep()] or [anim_prep_cat()]
#' and return the ggplot object. The user can still modify the plot the same as normal.
#'
#' @param object The animbook object returned from the prep function.
#' @param palette The vector of the palette used by the function to supply the color to each group.
#' @param rendering The choice of method used to create and display the plot, either gganimate or
#' plotly.
#' @param ... Additional arguments for customization, see details for more information.
#'
#' @return Return a ggplot object
#'
#' @details
#' This function takes prepared data and generates a ggplot object.
#' The kangaroo plot is the plot that shows the movement between groups over time.
#' The jitter point can be controlled using additional arguments such as height, width, and size
#' to control the appearance. For the shading area, the alpha argument can be used.
#'
#' @examples
#' animbook <- anim_prep(data = osiris, id = ID, values = sales, time = year, color = japan)
#'
#' kangaroo_plot(animbook)
#'
#' @importFrom ggplot2 element_blank
#'
#' @export

kangaroo_plot <- function(object,
                          palette = RColorBrewer::brewer.pal(9, "Set1"),
                          rendering = "ggplot",
                          ...) {

  col_val <- palette

  rendering_choice <- c("ggplot", "plotly")

  stopifnot("Use the anim_prep function to convert data into an categorized class object" =
              class(object) == "categorized",
            "The rendering argument can only be either ggplot or plotly" =
              rendering %in% rendering_choice)


# ... arguments -----------------------------------------------------------

  args <- list(...)

  # height settings for geom_jitter
  height <- 0.6

  if (!is.null(args[["height"]])) {
    height <- args[["height"]]
  }

  # width settings for geom_jitter
  width <- 50L

  if (!is.null(args[["width"]])) {
    width <- args[["width"]]
  }

  # alpha settings for paths shading
  alpha <- 0.1

  if (!is.null(args[["alpha"]])) {
    alpha <- args[["alpha"]]
  }

  # size settings for geom_jitter
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

  object <- kangaroo_data(object, height = height, width = width)


# variable main aes() -----------------------------------------------------

  if (rendering == "ggplot") {

    if ("color" %in% colnames(object[["data"]])) {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        group = quote(id),
        color = quote(color)
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

    if ("color" %in% colnames(object[["data"]])) {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        color = quote(color),
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
    ggplot2::geom_jitter(data = object[["data"]],
                         mapping = ggplot2::aes(!!!aes_list),
                         size = size
                         ) |>
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

  message("You can now used the animbook::anim_animate() function to transforemd it
          to an animated object.")

  class(anim) <- c("ggplot", "gg", "animated", "kangaroo")

  return(anim)

}












#' Kangaroo plot data
#'
#' This function performs data manipulation and formatting tasks
#' from the original object with additional data components for labeling and shading.
#'
#' @param object An animbook object
#'
#' @return A modified animbook object with additional data components
#'
#' @details The function takes the animbook object and creates a new label data and
#' shading data used for the plot then appends them to the original object.
#'
#' @keywords internal

kangaroo_data <- function(object,
                          height = 0.6,
                          width = 50) {


# stop --------------------------------------------------------------------

  stopifnot("height argument only accepted proportion between 0 and 1" =
              dplyr::between(height, 0, 1),
            "Please use the prep function to convert the data into categorized class object" =
              class(object) == "categorized")

  pre_data <- object[["data"]]

  x <- unique(pre_data$time)

  y <- sort(unique(pre_data$qtile), decreasing = TRUE)

  gap <- object[["settings"]]$gap

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


# create a path -----------------------------------------------------------

  path <- pre_data |>
    dplyr::group_nest(id) |>
    dplyr::mutate(draw = purrr::map(data, ~kangaroo_draw(.))) |>
    dplyr::select(id, draw) |>
    tidyr::unnest(cols = draw) |>
    dplyr::group_by(id)


# calculate frame ---------------------------------------------------------

  if (object[["settings"]]$time_dependent == TRUE) {
    data <- path
  }

  if (object[["settings"]]$time_dependent == FALSE) {
    data <- path |>
      dplyr::mutate(
        frame = frame + floor(stats::runif(1,
                                           1,
                                           width*10))
      )
  }


# join the missing information --------------------------------------------

  information <- pre_data |>
    dplyr::select(-c(time, qtile)) |>
    dplyr::distinct() |>
    stats::na.omit()

  kangaroo_data <- data |>
    dplyr::left_join(information,
                     by = "id") |>
    dplyr::mutate(y = y + stats::runif(1, -0.5 * (height/2), 0.5 * (height/2)))


# output ------------------------------------------------------------------

  object[["data"]] <- kangaroo_data

  kangaroo <- append(object, list(label_data, shade_data))

  names(kangaroo) <- c("data", "settings", "label_data", "shade_data")

  return(kangaroo)

}














#' Kangaroo path map
#'
#' This function is used to interpolate the path for the kangaroo plot.
#'
#' @param df A data frame
#'
#' @return A mapped data
#'
#' @keywords internal

kangaroo_draw <- function(df) {

  data <- df |>
    dplyr::arrange(time) |>
    dplyr::mutate(next_qtile = dplyr::lead(qtile, n = 1),
                  time_end = dplyr::lead(time, n = 1)) |>
    stats::na.omit()

  map <- purrr::map_dfr(seq_len(nrow(data)),
                 ~ sigmoid(as.numeric(data[.x, "time"]), as.numeric(data[.x, "time_end"]),
                        as.numeric(data[.x, "qtile"]), as.numeric(data[.x, "next_qtile"]),
                        n = 100) |>
                   dplyr::mutate(path_id = .x)) |>
    dplyr::mutate(frame = dplyr::row_number())

  return(map)

}

