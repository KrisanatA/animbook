#' @importFrom dplyr group_by arrange mutate select ungroup

prep_anim <- function(data, id = NULL, values = NULL, time = NULL, ngroup = 5) {

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)

  type <- sapply(data, class)[[as_label(qvalues)]]

  if (type == "numeric") {
    return(
      data |>
        # assigned the frame for each row which will be uses for the transition
        arrange(!!qid, !!qtime) |>
        group_by(!!qid) |>
        mutate(frame = row_number(),
               frame = frame + floor(runif(1, 1, 100))) |>
        ungroup() |>
        # split the values into groups using cut and quantile
        group_by(!!qtime) |>
        # ranking the variable of interest
        mutate(rank = as.integer(rank(!!qvalues)),
               # if the values is NA then rank will also be NA
               rank = ifelse(is.na(!!qvalues), NA, rank)) |>
        ungroup() |>
        # assign the y-axis values of the plot to each row based on their ranking quantile
        mutate(
          # using the cut function to assign the values to each row
          qtile = cut(rank,
                      stats::quantile(rank,
                                      probs = seq(0, 1, 1/ngroup),
                                      # do not count the NA values
                                      na.rm = TRUE),
                      include.lowest = TRUE,
                      labels = rev(seq(1, ngroup, 1))),
          # if the qtiles is NA then assign it to 0 otherwise converted the qtile from factor to integer
          qtile = ifelse(is.na(qtile), 0, as.integer(levels(qtile)[qtile]))
          ) |>
        ungroup()
    )
  }

  else {
    rlang::abort("The values must be of a numeric class")
  }

}
