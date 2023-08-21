prep_cat <- function(data,
                     id = NULL,
                     values = NULL,
                     time = NULL,
                     order = NULL,
                     time_dependent = FALSE,
                     runif_min = 1,
                     runif_max = 50) {

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)

  time_data <- data[, as_label(qtime)]

  type <- sapply(data, class)[[rlang::as_label(qvalues)]]

  stopifnot("This function only accepted categorical variable" = type %in% c("factor", "character"))

  n_group <- nrow(unique(data[, as_label(qvalues)]))

  if (is.null(order) | length(order) != n_group) {
    order <- data |>
      count(!!qvalues) |>
      arrange(n) |>
      pull(!!qvalues)
  }

  if (time_dependent == FALSE) {
    data_frame <- data |>
      arrange(!!qid, !!qtime) |>
      group_by(!!qid) |>
      mutate(
        frame = dplyr::row_number()
      ) |>
      ungroup()
  }

  if (time_dependent == TRUE) {
    data_frame <- data |>
      arrange(!!qid, !!qtime) |>
      group_by(!!qid) |>
      mutate(
        frame = dplyr::row_number(),
        frame = frame + floor(runif(1, runif_min, runif_max))
      ) |>
      ungroup()
  }

# order the factor --------------------------------------------------------

  book <- data_frame |>
    mutate(
      qtile = factor(!!qvalues, levels = order),
      qtile = ifelse(is.na(qtile), 0, as.numeric(qtile)),
      .keep = "unused"
    )

  return(book)

}
