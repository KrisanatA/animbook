#' Prepare Numerical Data into categorized data
#'
#' This function prepares the numerical data into the categorized format by
#' grouping data, scaling values, and creating necessary data and settings for the plot function.
#'
#' @param data A data frame containing the data to be prepared for visualization.
#' @param id The column name that represents the unique identifier variable.
#' @param values The column name that contains the numeric values to be visualized.
#' @param time The column name the represents the time variable.
#' @param ngroup The number of groups or categories to create for scaling values.
#' @param breaks A vector of breaks for creating bins.
#' @param group_scaling The column name that represents the grouping variable.
#' @param scaling The scaling method to be used; "rank" or "absolute".
#' @param time_dependent Logical. Should the visualization be time-dependent? The default is FALSE
#' @param color The column name to be used in [ggplot2::aes()] for the plot function.
#' @param label A vector of labels to represented each values.
#'
#' @return An animbook object:
#'  \item{data}{A data frame in the categorized format}
#'  \item{settings}{A list of settings to be used in the plot function, including gap,
#'  xbreaks, label, scaling, time_dependent}
#'
#' @details
#' The function takes the input data and performs several operations to prepare it for visualizations.
#' It groups data, scales values, and transformed it to categorized data along with the settings
#' for the plot function.
#'
#' @examples
#' anim_prep(data = osiris, id = ID, values = sales, time = year)
#'
#' anim_prep(data = osiris, id = ID, values = sales, time = year,
#' group_scaling = country)
#'
#' anim_prep(data = osiris, id = ID, values = sales, time = year,
#' scaling = "absolute")
#'
#' anim_prep(data = osiris, id = ID, values = sales, time = year,
#' group_scaling = country, scaling = "absolute")
#'
#' @export

anim_prep <- function(data,
                      id = NULL,
                      values = NULL,
                      time = NULL,
                      ngroup = 5L,
                      breaks = NULL,
                      group_scaling = NULL,
                      scaling = "rank",
                      time_dependent = FALSE,
                      color = NULL,
                      label = NULL) {


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
            "The id column needs to be a factor variable" =
              type[[rlang::as_label(qid)]] == "factor",
            "The values column needs to be a numeric variable. If the values
            the column is a category variable, try the anim_prep_cat function" =
              type[[rlang::as_label(qvalues)]] == "numeric",
            "The time column needs to be an integer variable" =
              type[[rlang::as_label(qtime)]] == "integer")


# scaling choices ---------------------------------------------------------

  scaling_choice <- c("rank", "absolute")

  stopifnot("The scaling can either be rank or absolute" =
              scaling %in% c(scaling_choice))


# group_scaling scale -----------------------------------------------------

  if (rlang::as_label(qgroup_scaling) != "NULL") {

    stopifnot("The group_scaling column needs to be a factor variable" =
                type[[rlang::as_label(qgroup_scaling)]] == "factor")

    if (scaling == "rank") {

      gdata_frame <- data |>
        dplyr::group_by(!!qgroup_scaling, !!qtime)

    }

    if (scaling == "absolute") {

      gdata_frame <- data |>
        dplyr::group_by(!!qgroup_scaling)

    }

  }

  if (rlang::as_label(qgroup_scaling) == "NULL") {

    if (scaling == "rank") {

      gdata_frame <- data |>
        dplyr::group_by(!!qtime)

    }

    if (scaling == "absolute") {

      gdata_frame <- data

    }

  }


# assign the qtile --------------------------------------------------------

  # rank scaling
  if (scaling == "rank") {

    book <- gdata_frame |>
      # ranking the variable of interest
      dplyr::mutate(
        rank = as.integer(rank(!!qvalues)),
        rank = ifelse(is.na(!!qvalues), NA, rank),
        .keep = "unused"
      )

    # default setting for breaks
    if (is.null(breaks)) {

      min <- 0
      max <- 1

      breaks <- seq(min, max, by = (max - min)/ngroup)

    }

    # if the breaks vector is provided
    else {

      stopifnot("The breaks argument only accepted vector" =
                  is.vector(breaks),
                "The breaks vector must have the same number of groups as ngroup argument" =
                  length(breaks) - 1 == ngroup,
                "The breaks vector should not contain NA" =
                  !is.na(breaks),
                "The breaks values must be between 0 and 1" =
                  all(dplyr::between(breaks, 0, 1))
      )

      breaks <- sort(breaks)

    }

    book <- book |>
      dplyr::mutate(
        min = ifelse(is.na(rank), NA, min(rank, na.rm = TRUE)),
        max = ifelse(is.na(rank), NA, max(rank, na.rm = TRUE)),
        normalize = (rank - min) / (max - min)
      ) |>
      dplyr::ungroup(!!qtime) |>
      # split the rank into equal size bins
      dplyr::mutate(
        qtile = cut(normalize,
                    breaks,
                    include.lowest = TRUE,
                    labels = rev(seq(1, ngroup, 1))),
        qtile = ifelse(is.na(qtile), 0, as.integer(levels(qtile)[qtile])),
        .keep = "unused"
      ) |>
      dplyr::ungroup()

  }

  # absolute scaling
  if (scaling == "absolute") {

    # default setting for breaks
    if (is.null(breaks)) {

      min <- 0
      max <- 1

      breaks <- seq(min, max, by = (max - min)/ngroup)

    }

    # if the breaks vector is provided
    else {

      stopifnot("The breaks argument only accepted vector" =
                  is.vector(breaks),
                "The breaks vector must have the same number of groups as ngroup argument" =
                  length(breaks) - 1 == ngroup,
                "The breaks vector should not contain NA" =
                  !is.na(breaks),
                "The breaks values must be between 0 and 1" =
                  all(dplyr::between(breaks, 0, 1))
      )

      breaks <- sort(breaks)

    }

    book <- gdata_frame |>
      dplyr::mutate(
        min = min(!!qvalues, na.rm = TRUE),
        max = max(!!qvalues, na.rm = TRUE),
        normalize = (!!qvalues - min) / (max - min)
      ) |>
      dplyr::mutate(
        qtile = cut(normalize,
                    breaks,
                    include.lowest = TRUE,
                    labels = rev(seq(1, ngroup, 1))),
        qtile = ifelse(is.na(qtile), 0, as.integer(levels(qtile)[qtile])),
        .keep = "unused"
      ) |>
      dplyr::ungroup()

  }


# return the selected columns with the name changes -----------------------

  name <- tibble::tibble(
    old = c(rlang::as_label(qid), rlang::as_label(qtime),
            rlang::as_label(qgroup_scaling), rlang::as_label(qcolor)),
    new = c("id", "time", "group", "color")
    )

  rename_vec <- stats::setNames(name$old, name$new)

  args_select <- c(rlang::as_label(qid),
                   rlang::as_label(qtime),
                   "qtile")

  # if group_scaling is not the same as color
  if (rlang::as_label(qgroup_scaling) != rlang::as_label(qcolor)) {

    # check if group_scaling is null or not
    if (rlang::as_label(qgroup_scaling) != "NULL") {

      args_select <- c(args_select, rlang::as_label(qgroup_scaling))

    }

    # check if color is null or not
    if (rlang::as_label(qcolor) != "NULL") {

      args_select <- c(args_select, rlang::as_label(qcolor))

    }

    # return animbook
    animbook <- book |>
      dplyr::select(args_select) |>
      dplyr::rename(tidyselect::any_of(rename_vec))

  }

  # if group_scaling is the same as color
  if (rlang::as_label(qgroup_scaling) == rlang::as_label(qcolor)) {

    # if both is null
    if (rlang::as_label(qgroup_scaling) == "NULL") {

      animbook <- book |>
        dplyr::select(args_select) |>
        dplyr::rename(tidyselect::any_of(rename_vec))

    }

    # if both is not null
    else {

      args_select <- c(args_select, rlang::as_label(qgroup_scaling))

      animbook <- book |>
        dplyr::select(args_select) |>
        dplyr::rename(tidyselect::any_of(rename_vec)) |>
        dplyr::mutate(color = group)

    }

  }


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

    warning("The length of the label provided is less than the length of y")
  }


# return an animbook object -----------------------------------------------

  object <- list(data = animbook,
                 settings = list(
                   gap = gap,
                   xbreaks = x,
                   label = as.character(label),
                   breaks_scales = breaks,
                   time_dependent = time_dependent
                 ))

  class(object) <- "categorized"

  message("You can now pass the object to the plot function.")

  return(object)

}
