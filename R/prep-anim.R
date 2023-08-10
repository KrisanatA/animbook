#'Prep the data structure for plot function
#'
#'This function prepared the data into the format the [anim_plot()] required.
#'
#'@param data The data frame or a tibble which contains the information of interested.
#'@param id Name of the column that uniquely identified the elements of interested
#'@param values Name of the column to be rank and assigned the group, must be a numeric column.
#'@param time Name of the columns that will be used as time variable.
#'@param ngroup Number of group the user want to split the values into (excluded
#'the NA group which is the additional group).
#'@param time_dependent Default for this function is FALSE, this means each id
#'will enter the animation at the different time
#'
#'@return Return a tibble with additional frame and qtile columns (excluded the
#'values columns which been used to calculated the qtile column)
#'
#'@examples
#'prep_anim(data = osiris, id = firmID, values = sales, time = year, ngroup = 5)
#'
#'@importFrom dplyr group_by arrange mutate select ungroup
#'@export

prep_anim <- function(data,
                      id = NULL,
                      values = NULL,
                      time = NULL,
                      ngroup = 5,
                      time_dependent = FALSE) {

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)

  type <- sapply(data, class)[[rlang::as_label(qvalues)]]

  n_data <- nrow(unique(data[, as_label(qid)]))

  stopifnot("This function only accepted numerical values" = type %in% c("numeric", "integer"))

  if (ngroup > n_data) {
    ngroup <- n_data
  }


# assign the frame --------------------------------------------------------

  if (time_dependent == FALSE) {
    data_frame <- data |>
      arrange(!!qid, !!qtime) |>
      group_by(!!qid) |>
      mutate(
        frame = dplyr::row_number(),
        # will need to make the size of the runif be automatically
        frame = frame + floor(runif(1, 1, 50))
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


# assign the qtile --------------------------------------------------------

  book <- data_frame |>
    group_by(!!qtime) |>
    # ranking the variable of interest
    mutate(
      rank = as.integer(rank(!!qvalues)),
      rank = ifelse(is.na(!!qvalues), NA, rank),
      .keep = "unused") |>
    ungroup() |>
    # assign the y-axis values of the plot to each row based on their ranking quantile
    mutate(
      qtile = cut(rank,
                  stats::quantile(rank,
                                  probs = seq(0, 1, 1/ngroup),
                                  na.rm = TRUE),
                  include.lowest = TRUE,
                  labels = rev(seq(1, ngroup, 1))),
      qtile = ifelse(is.na(qtile), 0, as.integer(levels(qtile)[qtile])),
      .keep = "unused") |>
    ungroup()

  return(book)

}
