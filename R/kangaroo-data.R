#' Kangaroo plot data
#'
#' This function performs data manipulation and path interpolation of the
#' categorized data. This includes additional data components for labeling
#' and shading.
#'
#' @param data The categorized data.
#' @param time_dependent Logical. Should the visualization be time-dependent?
#' The default is FALSE.
#' @param height The proportion the point takes in the shaded area.
#' @param width The number that controls the runif_max to specify how far apart
#' each point is.
#'
#' @return An object contained the modified data with additional data components.
#'
#' @details The function takes the categorized data and interpolates the path
#' for each observation. Additionally, the label and shading data are created.
#'
#' @keywords internal

kangaroo_data <- function(data,
                          time_dependent = FALSE,
                          height = 0.6,
                          width = 50L)  {


  # check -------------------------------------------------------------------

  stopifnot("height argument only accepted proportion between 0 and 1" =
              dplyr::between(height, 0, 1),
            "Please use the prep function to convert the data into a categorized format" =
              any("categorized" %in% class(data)))

  class(data) <- c("tbl_df", "tbl", "data.frame")

  x <- unique(data$time)

  y <- sort(unique(data$qtile), decreasing = TRUE)

  label <- data |>
    dplyr::distinct(qtile, label) |>
    dplyr::arrange(dplyr::desc(qtile)) |>
    dplyr::pull(label)


  # gap settings ------------------------------------------------------------

  gap <- 0.1 * (length(x) - 1)


  # label data --------------------------------------------------------------

  label_data <- tibble::tibble(
    x = min(x) - (2 * gap),
    y = y,
    label = label
  )


  # create shading data -----------------------------------------------------

  shade_data <- tibble::tibble(
    id = y,
    xmin = rep(min(x) - gap, length(y)),
    xmax = rep(max(x) + gap, length(y)),
    ymin = y - 0.25,
    ymax = y + 0.25
  )


  # create a path -----------------------------------------------------------

  path <- data |>
    dplyr::group_nest(id) |>
    dplyr::mutate(draw = purrr::map(data, ~kangaroo_draw(.))) |>
    dplyr::select(id, draw) |>
    tidyr::unnest(cols = draw) |>
    dplyr::group_by(id)


  # calculate frame ---------------------------------------------------------

  # check time_dependent argument
  stopifnot("time_dependent argument only accepted logical values" =
              is.logical(time_dependent))

  if (time_dependent == TRUE) {
    animated <- path |>
      dplyr::mutate(
        frame = dplyr::row_number()
      )
  }

  if (time_dependent == FALSE) {

    max <- width*(length(x)-1)

    animated <- path |>
      dplyr::mutate(
        frame = dplyr::row_number(),
        frame = frame + floor(stats::runif(1,
                                           1,
                                           max))
      )
  }


  # join the missing information --------------------------------------------

  information <- data |>
    dplyr::select(-c(time, qtile)) |>
    dplyr::distinct() |>
    stats::na.omit()

  kangaroo_data <- animated |>
    dplyr::left_join(information,
                     by = "id") |>
    dplyr::mutate(y = y + stats::runif(1, -0.5 * (height/2), 0.5 * (height/2)))


  # output ------------------------------------------------------------------

  object <- list(data = kangaroo_data,
                 label_data = label_data,
                 shade_data = shade_data,
                 xbreaks = sort(x))

  return(object)

}



#' Kangaroo path map
#'
#' This function is used to interpolate the path for the kangaroo plot.
#'
#' @param df A data frame.
#'
#' @return A mapped data.
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
                                  n = 15) |>
                          dplyr::mutate(path_id = .x))

  return(map)

}
