#' @importFrom dplyr group_by arrange mutate select ungroup

prep_anim <- function(data, id = NULL, values = NULL, time = NULL, ngroup = 5) {

  qid <- enquo(id)
  qvalues <- enquo(values)
  qtime <- enquo(time)

  type <- sapply(data, class)[[as_label(qvalues)]]

  if (type == "numeric") {
    return(
      data |>
        # assigned the frame for each row which will be uses for the transition
        arrange(!!qid, !!qtime) |>
        mutate(frame = row_number(),
               frame = frame + floor(runif(1, 1, 100))) |>
        # split the values into groups using cut and quantile
        group_by(!!qtime) |>
        mutate(qtile = cut(!!qvalues,
                           stats::quantile(!!qvalues,
                                           probs = seq(0, 1, 1/ngroup)),
                           include.lowest = TRUE,
                           labels = seq(1, ngroup, 1))) |>
        ungroup()
    )
  }

  else {
    abort("The values must be of a numeric class")
  }

}
