#'Prepare Data for Animated Visualizations
#'
#'This function prepares the data into the format the [anim_plot()] required by assigning frames,
#'grouping data, scaling values, and creating a necessary data and settings for the [anim_plot()] function.
#'
#'@param data A data frame containing the data to be prepared for visualization.
#'@param id The column name that represents the unique identifier variable.
#'@param values The column name that contains the numeric values to be visualized.
#'@param time The column name representing the time variable.
#'@param label A vector of labels to be used for the y-axis in the visualization.
#'@param ngroup The number of groups or categories to create for scaling values.
#'@param breaks A vector of breaks for creating bins.
#'@param group_scaling The column name that represents the grouping variable.
#'@param color The column name to used in [aes()] for the [anim_plot()].
#'@param time_dependent Logical. Should the visualization be time-dependent? Default is TRUE.
#'@param scaling The scaling method to be used: "rank" or "absolute".
#'@param runif_min The minimum value for random addition to frame numbers.
#'@param runif_max The maximum value for random addition to frame numbers.
#'
#'@return An animbook object:
#'  \item{data}{A data frame with prepared data for visualization.}
#'  \item{settings}{A list of settings to be used in [anim_plot()], including data, gap, xbreaks, label and scaling.}
#'
#'@details
#'The function takes the input data and performs several operations to prepare it for visualizations.
#'It assigns frames, groups data, scales values, and creates necessary data and settings for the [anim_plot()]
#'function.
#'
#'@examples
#'anim_prep(data = osiris, id = ID, values = sales, time = year)
#'
#'anim_prep(data = osiris, id = ID, values = sales, time = year, group_scaling = country)
#'
#'anim_prep(data = osiris, id = ID, values = sales, time = year, scaling = "absolute")
#'
#'anim_prep(data = osiris, id = ID, values = sales, time = year, group_scaling = country, scaling = "absolute")
#'
#'@importFrom dplyr group_by arrange mutate select ungroup arrange
#'@export

anim_prep <- function(data,
                      id = NULL,
                      values = NULL,
                      time = NULL,
                      label = NULL,
                      ngroup = 5L,
                      breaks = NULL,
                      group_scaling = NULL,
                      color = NULL,
                      time_dependent = TRUE,
                      scaling = "rank",
                      runif_min = 1,
                      runif_max = 50) {


# enquo -------------------------------------------------------------------

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)
  qgroup_scaling <- rlang::enquo(group_scaling)
  qcolor <- rlang::enquo(color)


# check column class ------------------------------------------------------

  type <- sapply(data, class)

  stopifnot("The id, values, and time columns need to be specified" =
              rlang::as_label(qid) != "NULL" &
              rlang::as_label(qvalues) != "NULL" &
              rlang::as_label(qtime) != "NULL",
            "The id column need to be factor variable" =
              type[[rlang::as_label(qid)]] == "factor",
            "The values column need to be numeric variable" =
              type[[rlang::as_label(qvalues)]] == "numeric",
            "The time column need to be integer variable" =
              type[[rlang::as_label(qtime)]] == "integer")


# scaling choice ----------------------------------------------------------

  scaling_choice <- c("rank", "absolute")

  stopifnot("The scaling can either be rank or absolute" =
              scaling %in% c(scaling_choice))


# assign the frames -------------------------------------------------------

  if (time_dependent == FALSE) {

    data_frame <- data |>
      arrange(!!qid, !!qtime) |>
      group_by(!!qid) |>
      mutate(
        frame = dplyr::row_number(),
        frame = frame + floor(runif(1, runif_min, runif_max))
      ) |>
      ungroup()

  }

  if (time_dependent == TRUE) {

    data_frame <- data |>
      arrange(!!qid, !!qtime) |>
      group_by(!!qid) |>
      mutate(
        frame = dplyr::row_number()
      ) |>
      ungroup()

  }


# group_scaling scale -------------------------------------------------------------


  if (rlang::as_label(qgroup_scaling) != "NULL") {

    stopifnot("The group_scaling column need to be factor variable" =
                type[[rlang::as_label(qgroup_scaling)]] == "factor")

    if (scaling == "rank") {

      gdata_frame <- data_frame |>
        group_by(!!qgroup_scaling, !!qtime)

    }

    if (scaling == "absolute") {

      gdata_frame <- data_frame |>
        group_by(!!qgroup_scaling)

    }

  }

  if (rlang::as_label(qgroup_scaling) == "NULL") {

    if (scaling == "rank") {

      gdata_frame <- data_frame |>
        group_by(!!qtime)

    }

    if (scaling == "absolute") {

      gdata_frame <- data_frame

    }

  }



# assign the qtile --------------------------------------------------------

  # rank scaling
  if (scaling == "rank") {

    book <- gdata_frame |>
      # ranking the variable of interest
      mutate(
        rank = as.integer(rank(!!qvalues)),
        rank = ifelse(is.na(!!qvalues), NA, rank),
        .keep = "unused"
      ) |>
      ungroup()

    # default setting for breaks
    if (is.null(breaks)) {

      breaks <- stats::quantile(book$rank,
                                probs = seq(0, 1, 1/ngroup),
                                na.rm = TRUE)

    }

    # if the breaks vector is provided
    else {

      stopifnot("The breaks argument only accepted vector" =
                  is.vector(breaks),
                "The breaks vector must have the same number of group as ngroup argument" =
                  length(breaks) - 1 == ngroup,
                "The breaks vector should not contains NA" =
                  !is.na(breaks),
                "The breaks values must be between 0 and 1" =
                  all(dplyr::between(breaks, 0, 1))
      )

    breaks <- stats::quantile(book$rank,
                              probs = sort(breaks),
                              na.rm = TRUE)

    }




    book <- book |>
      # split the rank into equal size bins
      mutate(
        qtile = cut(rank,
                    breaks,
                    include.lowest = TRUE,
                    labels = rev(seq(1, ngroup, 1))),
        qtile = ifelse(is.na(qtile), 0, as.integer(levels(qtile)[qtile])),
        .keep = "unused"
      )

  }

  # absolute scaling
  if (scaling == "absolute") {

    # default setting for breaks
    if (is.null(breaks)) {

      vector <- dplyr::pull(data, !!qvalues)

      min <- min(vector, na.rm = TRUE)
      max <- max(vector, na.rm = TRUE)

      breaks <- seq(min, max, by = (max - min)/ngroup)

    }

    # if the breaks vector is provided
    else {

      stopifnot("The breaks argument only accepted vector" =
                  is.vector(breaks),
                "The breaks vector must have the same number of group as ngroup argument" =
                  length(breaks) - 1 == ngroup,
                "The breaks vector should not contains NA" =
                  !is.na(breaks),
                "The breaks values is not in the range of values" =
                  all(dplyr::between(breaks,
                                     min(data[, rlang::as_label(!!qvalues)]),
                                     max(data[, rlang::as_label(!!qvalues)])))
                )

      breaks <- sort(breaks)

    }

    book <- gdata_frame |>
      mutate(
        qtile = cut(!!qvalues,
                    breaks,
                    include.lowest = TRUE,
                    labels = rev(seq(1, ngroup, 1))),
        qtile = ifelse(is.na(qtile), 0, as.integer(levels(qtile)[qtile])),
        .keep = "unused"
      ) |>
      ungroup()

  }


# return the selected columns with name changes ---------------------------

  args_select <- c(rlang::as_label(qid),
                   rlang::as_label(qtime),
                   "qtile",
                   "frame")

  if (rlang::as_label(qgroup_scaling) != "NULL") {

    args_select <- c(args_select, rlang::as_label(qgroup_scaling))

  }

  if (rlang::as_label(qcolor) != "NULL") {

    args_select <- c(args_select, rlang::as_label(qcolor))

  }

  name <- tibble::tibble(
    old = c(rlang::as_label(qid), rlang::as_label(qtime),
            rlang::as_label(qgroup_scaling), rlang::as_label(qcolor)),
    new = c("id", "time", "group", "color")
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
    label <- as.character(y)
  }

  if (length(label) >= length(y)) {
    label <- label[1:length(y)]
  }

  if (length(label) < length(y)) {
    label <- as.character(y)

    warning("length of the label provided is less than length of y")
  }


# return a list -----------------------------------------------------------

  object <- list(data = animbook,
                 settings = list(
                   gap = gap,
                   xbreaks = x,
                   label = label,
                   scaling = breaks
                 ))

  class(object) <- "animbook"

  return(object)

}






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
      ungroup()

  }

  if (time_dependent == TRUE) {

    data_frame <- data |>
      dplyr::arrange(!!qid, !!qtime) |>
      dplyr::group_by(!!qid) |>
      dplyr::mutate(
        frame = dplyr::row_number()
      ) |>
      ungroup()

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
                all(order %in% unique(data[, rlang::as_label(qvalues)]))
    )

    order <- order

  }


# assign the qtile --------------------------------------------------------

  book <- data_frame |>
    dplyr::mutate(
      qtile = factor(!!qvalues, levels = order),
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
                   label = rev(label),
                   order = rev(order)
                 ))

  class(object) <- "animbook"

  return(object)

}

