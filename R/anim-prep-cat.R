#' Transformed category data into categorized format
#'
#' This function transformed the category data in the categorized format by
#' ordering the values.
#'
#' @param data A data frame contained the category values.
#' @param id The column name that represents the identifiers variable.
#' @param values The column name the contains the category values.
#' @param time The column name that represents the time variable.
#' @param group The column name that represents the distinguish group between
#' the values
#' @param order A vector of order for sorting the category values.
#' @param label A vector of labels to represented the qtile
#'
#' @return A categorized data
#'
#' @details
#' The function takes the input data, ordering the values, and assigning the
#' variable names.
#'
#' @examples
#' anim_prep_cat(data = aeles, id = id, values = party, time = year)
#'
#' @export

anim_prep_cat <- function(data,
                          id  = NULL,
                          values = NULL,
                          time = NULL,
                          group = NULL,
                          order = NULL,
                          label = NULL) {


  # enquo -------------------------------------------------------------------

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)
  qgroup <- rlang::enquo(group)


  # check column class ------------------------------------------------------

  type <- sapply(data, class)

  stopifnot("The id, values, and time columns need to be specified" =
              rlang::as_label(qid) != "NULL" &
              rlang::as_label(qvalues) != "NULL" &
              rlang::as_label(qtime) != "NULL",
            "The id column needs to be a factor variable" =
              type[[rlang::as_label(qid)]] == "factor",
            "The values column needs to be a factor variable. If the values column is a
            numerical variable, try using the anim_prep function" =
              type[[rlang::as_label(qvalues)]] == "factor",
            "The time column needs to be an integer variable" =
              type[[rlang::as_label(qtime)]] == "integer")


  # order -------------------------------------------------------------------

  ncat <- nrow(unique(data[, rlang::as_label(qvalues)]))

  # if order is NULL
  if (is.null(order)) {

    # order based on number of observation
    order <- data |>
      dplyr::count(!!qvalues) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::pull(!!qvalues)

  }

  # if order is not NULL
  else {

    # check
    stopifnot("The order argument only accepted vector" =
                is.vector(order),
              "The order vector must have the same number as the unique values element" =
                length(order) == ncat,
              "The order vector should not contain NA" =
                !is.na(order),
              "The order vector must be the elements of the values column" =
                all(order %in% unique(dplyr::pull(data, !!qvalues)))
    )

    order <- order

  }


  # assign the qtile --------------------------------------------------------

  # order the qtile
  order_data <- data |>
    dplyr::mutate(
      qtile = factor(!!qvalues, levels = rev(order)),
      qtile = ifelse(is.na(qtile), 0, as.numeric(qtile)),
      .keep = "unused"
    )


  # labels ------------------------------------------------------------------

  # if label is NULL
  if (is.null(label)) {
    label <- as.character(order)
  }

  # if label length is greater than number of category
  if (length(label) >= length(order)) {
    label <- label[1:length(order)]
  }

  # if label length is less than number of category
  if (length(label) < length(order)) {
    label <- as.character(order)

    warning("The length of the label provided is less than number of category")
  }

  label_lookup <- tibble::tibble(
    qtile = sort(unique(order_data$qtile), decreasing = TRUE),
    label = label
  )

  lab_data <- order_data |>
    dplyr::left_join(label_lookup,
                     by = "qtile")


  # return the selected columns with name changes ---------------------------

  args_select <- c(rlang::as_label(qid),
                   rlang::as_label(qtime),
                   "qtile",
                   "label")

  # if group is not NULL
  if (rlang::as_label(qgroup) != "NULL") {

    args_select <- c(args_select, rlang::as_label(qgroup))

  }

  name <- tibble::tibble(
    old = c(rlang::as_label(qid), rlang::as_label(qtime),
            rlang::as_label(qgroup)),
    new = c("id", "time", "group")
  )

  rename_vec <- stats::setNames(name$old, name$new)

  # categorized data
  categorized <- lab_data |>
    dplyr::select(args_select) |>
    dplyr::rename(tidyselect::any_of(rename_vec))

  class(categorized) <- c("tbl_df", "tbl", "data.frame", "categorized")

  return(categorized)

}
