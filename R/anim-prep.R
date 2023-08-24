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
#'@param breaks A vector of breaks for creating bins when using absolute scaling.
#'@param group The column name that represents the grouping variable.
#'@param time_dependent Logical. Should the visualization be time-dependent? Default is TRUE.
#'@param scaling The scaling method to be used: "rank" or "absolute".
#'@param runif_min The minimum value for random addition to frame numbers.
#'@param runif_max The maximum value for random addition to frame numbers.
#'
#'@return A list containing the following components:
#'  \item{data}{A data frame with prepared data for visualization.}
#'  \item{rect_data}{A data frame that wil be used for shading in [anim_plot()].}
#'  \item{settings}{A list of settings to be used in [anim_plot()], including gap, breaks, and labels.}
#'
#'@examples
#'prep_anim(data = osiris, id = firmID, values = sales, time = year)
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
                      group = NULL,
                      time_dependent = TRUE,
                      scaling = "rank",
                      runif_min = 1,
                      runif_max = 50) {


# enquo -------------------------------------------------------------------

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)
  qgroup <- rlang::enquo(group)


# check column class ------------------------------------------------------

  type <- sapply(data, class)

  stopifnot("The id column need to be factor variable" =
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


# group scale -------------------------------------------------------------

  if (!is.null(rlang::as_label(qgroup))) {

    stopifnot("The group column need to be factor variable" =
                type[[rlang::as_label(qgroup)]] == "factor")

    if (scaling == "rank") {

      gdata_frame <- data_frame |>
        group_by(!!qgroup, !!qtime)

    }

    if (scaling == "absolute") {

      gdata_frame <- data_frame |>
        group_by(!!qgroup)

    }

  }

  if (is.null(rlang::as_label(qgroup))) {

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
      ungroup() |>
      # split the rank into equal size bins
      mutate(
        qtile = cut(rank,
                    stats::quantile(rank,
                                    probs = seq(0, 1, 1/ngroup),
                                    na.rm = TRUE),
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

      breaks <- pretty(!!qvalues, ngroup)

    }

    # if the breaks vector is provided
    else {

      stopifnot("The breaks argument only accepted vector" =
                  is.vector(breaks),
                "The breaks vector must have a length greater than two" =
                  length(breaks) > 2,
                "The breaks vector should not contains NA" =
                  !is.na(breaks),
                "The breaks values is not in the range of values" =
                  all(dplyr::between(breaks,
                                     min(data[, rlang::as_label(!!qvalues)]),
                                     max(data[, rlang::as_label(!!qvalues)])))
                )

      breaks <- breaks

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


# rect data ---------------------------------------------------------------

  x <- dplyr::pull(unique(book[, rlang::as_label(qtime)]))

  y <- sort(unique(book$qtile), decreasing = TRUE)

  gap <- 0.1 * (length(x) - 1)

  rect_data <- data_frame(
    id = y,
    xmin = rep(min(x) - gap, length(y)),
    xmax = rep(max(x) + gap, length(y)),
    ymin = y - 0.25,
    ymax = y + 0.25
   )


# labels ------------------------------------------------------------------

  if (is.null(label)) {
    label <- as.character(y)
  }

  if (length(label) >= length(y)) {
    label <- label[1:length(y)]
  }

  if (length(label) < length(y)) {
    label <- as.character(y)

    warning("length of the label provided is less that length of y")
  }


# return a list -----------------------------------------------------------

  return(
    list(data = book,
         rect_data = rect_data,
         settings = list(
           gap = gap,
           breaks = x,
           labels = label
           )
         )
    )

}


