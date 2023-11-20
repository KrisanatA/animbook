#' Transformed numerical into categorized data
#'
#' This function transformed the numerical data into the categorized format by
#' grouping data and scaling values.
#'
#' @param data A data frame contained the numerical values.
#' @param id The column name that represents the identifiers variable.
#' @param values The column name the contains the numeric values.
#' @param time The column name that represents the time variable.
#' @param group The column name that represents the distinguish group between
#' the values
#' @param ncat The number of categories to be created for scaling values.
#' @param breaks A vector of breaks for creating bins.
#' @param label A vector of labels to represented the qtile
#' @param group_scaling The column name that will be used for grouping the
#' variable before scaling.
#' @param scaling The scaling method to be used; "rank" or "absolute".
#'
#' @return A categorized data
#'
#' @details
#' The function takes the input data and performs several operations to
#' transformed it into categorized format. It is done by groups data, scales
#' values, and assigned the qtile.
#'
#' @examples
#' # rank scaling
#' anim_prep(data = osiris, id = ID, values = sales, time = year)
#'
#' # group_rank scaling
#' anim_prep(data = osiris, id = ID, values = sales, time = year,
#' group_scaling = country)
#'
#' # absolute scaling
#' anim_prep(data = osiris, id = ID, values = sales, time = year,
#' scaling = "absolute")
#'
#' # group_absolute scaling
#' anim_prep(data = osiris, id = ID, values = sales, time = year,
#' group_scaling = country, scaling = "absolute")
#'
#' @export

anim_prep <- function(data,
                      id = NULL,
                      values = NULL,
                      time = NULL,
                      group = NULL,
                      ncat = 5L,
                      breaks = NULL,
                      label = NULL,
                      group_scaling = NULL,
                      scaling = "rank") {


  # enquo -------------------------------------------------------------------

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)
  qgroup <- rlang::enquo(group)
  qgroup_scaling <- rlang::enquo(group_scaling)


  # check column class ------------------------------------------------------

  type <- sapply(data, class)

  stopifnot("The id, values, and time columns need to be specified" =
              rlang::as_label(qid) != "NULL" &
              rlang::as_label(qvalues) != "NULL" &
              rlang::as_label(qtime) != "NULL",
            "The id column needs to be a factor variable" =
              type[[rlang::as_label(qid)]] == "factor",
            "The values column needs to be a numeric variable. If the values column is a
            category variable, try using the anim_prep_cat function" =
              type[[rlang::as_label(qvalues)]] == "numeric",
            "The time column needs to be an integer variable" =
              type[[rlang::as_label(qtime)]] == "integer")


  # scaling choices ---------------------------------------------------------

  scaling_choice <- c("rank", "absolute")

  stopifnot("The scaling can either be rank or absolute" =
              scaling %in% c(scaling_choice))


  # group_scaling scale -----------------------------------------------------

  # if group scaling is provided
  if (rlang::as_label(qgroup_scaling) != "NULL") {

    # check
    stopifnot("The group_scaling column needs to be a factor variable" =
                type[[rlang::as_label(qgroup_scaling)]] == "factor")

    # group_rank scaling
    if (scaling == "rank") {

      gdata_frame <- data |>
        dplyr::group_by(!!qgroup_scaling, !!qtime)

    }

    # group_absolute scaling
    if (scaling == "absolute") {

      gdata_frame <- data |>
        dplyr::group_by(!!qgroup_scaling)

    }

  }

  # if group_scaling is not provided
  if (rlang::as_label(qgroup_scaling) == "NULL") {

    # rank scaling
    if (scaling == "rank") {

      gdata_frame <- data |>
        dplyr::group_by(!!qtime)

    }

    # absolute scaling
    if (scaling == "absolute") {

      gdata_frame <- data

    }

  }


  # assign the qtile --------------------------------------------------------

  # rank scaling
  if (scaling == "rank") {

    # ranking the variable of interest
    rank <- gdata_frame |>
      dplyr::mutate(
        rank = as.integer(rank(!!qvalues)),
        rank = ifelse(is.na(!!qvalues), NA, rank),
        .keep = "unused"
      )

    # default settings for breaks
    if (is.null(breaks)) {

      min <- 0
      max <- 1

      breaks <- seq(min, max, by = (max - min)/ncat)

    }

    # if the breaks vector is provided
    if (!is.null(breaks)) {

      # check the vector
      stopifnot("The breaks argument only accepted vector" =
                  is.vector(breaks),
                "The breaks vector must have the same number of gouprs as ncat argument" =
                  length(breaks) - 1 == ncat,
                "The breaks vector should not contained NA" =
                  !is.na(breaks),
                "The breaks values must be between 0 and 1" =
                  all(dplyr::between(breaks, 0, 1))
      )

      breaks <- sort(breaks)

    }

    # rank categorized
    scaling_data <- rank |>
      dplyr::mutate(
        min = ifelse(is.na(rank), NA, min(rank, na.rm = TRUE)),
        max = ifelse(is.na(rank), NA, max(rank, na.rm = TRUE)),
        normalize = (rank - min) / (max - min)
      ) |>
      dplyr::ungroup(!!qtime) |>
      # splite the rank into bins
      dplyr::mutate(
        qtile = cut(normalize,
                    breaks,
                    include.lowest = TRUE,
                    labels = rev(seq(1, ncat, 1))),
        qtile = ifelse(is.na(qtile), 0, as.integer(levels(qtile)[qtile])),
        .keep = "unused"
      ) |>
      dplyr::ungroup()

  }

  # absolute scaling
  if (scaling == "absolute") {

    # default settings for breaks
    if (is.null(breaks)) {

      min <- 0
      max <- 1

      breaks <- seq(min, max, by = (max - min)/ncat)

    }

    # if the breaks vector is provided
    if (!is.null(breaks)) {

      # check the vector
      stopifnot("The breaks argument only accepted vector" =
                  is.vector(breaks),
                "The breaks vector must have the same number of gouprs as ncat argument" =
                  length(breaks) - 1 == ncat,
                "The breaks vector should not contained NA" =
                  !is.na(breaks),
                "The breaks values must be between 0 and 1" =
                  all(dplyr::between(breaks, 0, 1))
      )

      breaks <- sort(breaks)

    }

    # absolute categorized
    scaling_data <- gdata_frame |>
      dplyr::mutate(
        min = min(!!qvalues, na.rm = TRUE),
        max = max(!!qvalues, na.rm = TRUE),
        normalize = (!!qvalues - min) / (max - min)
      ) |>
      # split the values into bins
      dplyr::mutate(
        qtile = cut(normalize,
                    breaks,
                    include.lowest = TRUE,
                    labels = rev(seq(1, ncat, 1))),
        qtile = ifelse(is.na(qtile), 0, as.integer(levels(qtile)[qtile])),
        .keep = "unused"
      ) |>
      dplyr::ungroup()

  }

  # labels ------------------------------------------------------------------

  y <- sort(unique(scaling_data$qtile), decreasing = TRUE)

  # if label is NULL
  if (is.null(label)) {
    label <- as.character(y)
  }

  # if label length is greater than number of category
  if (length(label) >= length(y)) {
    label <- label[1:length(y)]
  }

  # if label length is less than number of category
  if (length(label) < length(y)) {
    label <- as.character(y)

    warning("The length of the label provided is less than number of category")
  }

  label_lookup <- tibble::tibble(
    qtile = sort(unique(scaling_data$qtile), decreasing = TRUE),
    label = label
  )

  lab_data <- scaling_data |>
    dplyr::left_join(label_lookup,
                     by = "qtile")


  # return the selected columns with the name changes -----------------------

  name <- tibble::tibble(
    old = c(rlang::as_label(qid), rlang::as_label(qtime),
            rlang::as_label(qgroup), rlang::as_label(qgroup_scaling)),
    new = c("id", "time", "group", "group_scaling")
  )

  rename_vec <- stats::setNames(name$old, name$new)

  args_select <- c(rlang::as_label(qid),
                   rlang::as_label(qtime),
                   "qtile",
                   "label")

  # if group_scaling is not the same as group
  if (rlang::as_label(qgroup_scaling) != rlang::as_label(qgroup)) {

    # if group_scaling is not NULL
    if (rlang::as_label(qgroup_scaling) != "NULL") {

      args_select <- c(args_select, rlang::as_label(qgroup_scaling))

    }

    # if group is not NULL
    if (rlang::as_label(qgroup) != "NULL") {

      args_select <- c(args_select, rlang::as_label(qgroup))

    }

    # categorized data
    categorized <- lab_data |>
      dplyr::select(args_select) |>
      dplyr::rename(tidyselect::any_of(rename_vec))

  }

  # if group_scaling is the same as group
  if (rlang::as_label(qgroup_scaling) == rlang::as_label(qgroup)) {

    # if both is NULL
    if (rlang::as_label(qgroup_scaling) == "NULL") {

      # categorized data
      categorized <- lab_data |>
        dplyr::select(args_select) |>
        dplyr::rename(tidyselect::any_of(rename_vec))

    }

    # if both is not NULL
    if (rlang::as_label(qgroup_scaling) != "NULL") {

      args_select <- c(args_select, rlang::as_label(qgroup_scaling))

      # categorized data
      categorized <- lab_data |>
        dplyr::select(args_select) |>
        dplyr::rename(tidyselect::any_of(rename_vec)) |>
        dplyr::mutate(group = group_scaling)

    }
  }

  class(categorized) <- c("tbl_df", "tbl", "data.frame", "categorized")

  return(categorized)

}
