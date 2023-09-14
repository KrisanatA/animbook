#'Prepare Category Data for Animated Visualizations
#'
#'This function prepares the category data into the format the [anim_plot()] required
#'by assigning frames and creating a necessary data and settings for the [anim_plot()] function.
#'
#'@param data A data frame containing the category values to be prepared for visualization.
#'@param id The column name that represents the unique identifier variable.
#'@param values The column name that contains the categorical values to be visualized.
#'@param time The column name representing the time variable.
#'@param label A vector of labels to be used for the y-axis in the visualization.
#'@param order A vector of order for sorting the category values.
#'@param color The column name to be used in [ggplot2::aes()] for the [anim_plot()].
#'@param time_dependent Logical. Should the visualization be time-dependent? Default is TRUE.
#'@param runif_min The minimum value for random addition to frame numbers.
#'@param runif_max The maximum value for random addition to frame numbers.
#'
#'@return An animbook object:
#'  \item{data}{A data frame with prepared data for visualisation.}
#'  \item{settings}{A list of settings to be used in [anim_plot()], including gap, xbreaks, label and scaling}
#'
#'@details
#'The function takes the input data and performs several operations to prepare it for visualizations.
#'It assigns frames and create necessary data and settings for the [anim_plot()] function.
#'
#'@examples
#'anim_prep_cat(data = aeles, id = id, values = party, time = year)
#'
#'@export

anim_prep_cat <- function(data,
                          id = NULL,
                          values = NULL,
                          time = NULL,
                          label = NULL,
                          order = NULL,
                          color = NULL,
                          time_dependent = TRUE,
                          runif_min = 1,
                          runif_max = 50) {

# enquo -------------------------------------------------------------------

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)
  qcolor <- rlang::enquo(color)


# check column class ------------------------------------------------------

  type <- sapply(data, class)

  stopifnot("The id, values, and time columns need to be specified" =
              rlang::as_label(qid) != "NULL" &
              rlang::as_label(qvalues) != "NULL" &
              rlang::as_label(qtime) != "NULL",
            "The id column need to be factor variable" =
              type[[rlang::as_label(qid)]] == "factor",
            "The values column need to be factor variable" =
              type[[rlang::as_label(qvalues)]] == "factor",
            "The time column need to be integer variable" =
              type[[rlang::as_label(qtime)]] == "integer")


# assign the frames -------------------------------------------------------

  if (time_dependent == FALSE) {

    data_frame <- data |>
      dplyr::arrange(!!qid, !!qtime) |>
      dplyr::group_by(!!qid) |>
      dplyr::mutate(
        frame = dplyr::row_number(),
        frame = frame + floor(runif(1, runif_min, runif_max))
      ) |>
      dplyr::ungroup()

  }

  if (time_dependent == TRUE) {

    data_frame <- data |>
      dplyr::arrange(!!qid, !!qtime) |>
      dplyr::group_by(!!qid) |>
      dplyr::mutate(
        frame = dplyr::row_number()
      ) |>
      dplyr::ungroup()

  }

# order -------------------------------------------------------------------

  n_group <- nrow(unique(data[, rlang::as_label(qvalues)]))

  if (is.null(order)) {

    order <- data |>
      dplyr::count(!!qvalues) |>
      dplyr::arrange(n) |>
      dplyr::pull(!!qvalues)

  }

  else {

    stopifnot("The order argument only accepted vector" =
                is.vector(order),
              "The order vector must have the same number as the unique values element" =
                length(order) == n_group,
              "The breaks vector should not contains NA" =
                !is.na(order),
              "The order vector must be the elements of the values column" =
                all(order %in% dplyr::pull(data, !!qvalues))
    )

    order <- order

  }


 # assign the qtile --------------------------------------------------------

  book <- data_frame |>
    dplyr::mutate(
      qtile = factor(!!qvalues, levels = rev(order)),
      qtile = ifelse(is.na(qtile), 0, as.numeric(qtile)),
      .keep = "unused"
    )


 # return the selected columns with name changes ---------------------------

  args_select <- c(rlang::as_label(qid),
                   rlang::as_label(qtime),
                   "qtile",
                   "frame")

  if (rlang::as_label(qcolor) != "NULL") {

    args_select <- c(args_select, rlang::as_label(qcolor))

  }

  name <- tibble::tibble(
    old = c(rlang::as_label(qid), rlang::as_label(qtime),
            rlang::as_label(qcolor)),
    new = c("id", "time", "color")
  )

  rename_vec <- stats::setNames(name$old, name$new)

  animbook <- book |>
    dplyr::select(args_select) |>
    dplyr::rename(tidyselect::any_of(rename_vec))


# gap settings ------------------------------------------------------------

  x <- dplyr::pull(unique(book[, rlang::as_label(qtime)]))

  gap <- 0.1 * (length(x) - 1)


# labels ------------------------------------------------------------------

  y <- sort(unique(animbook$qtile), decreasing = TRUE)

  if (is.null(label)) {
    label <- order
  }

  if (length(label) >= length(y)) {
    label <- label[1:length(y)]
  }

  if (length(label) < length(y)) {
    label <- order

    warning("length of the label provided is less than length of y")
  }


 # return a list -----------------------------------------------------------

  object <- list(data = animbook,
                 settings = list(
                   gap = gap,
                   xbreaks = x,
                   label = label,
                   order = order
                 ))

  class(object) <- "animbook"

  return(object)

}
