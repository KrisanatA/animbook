#' @importFrom dplyr group_by arrange mutate select ungroup

prep_anim <- function(data, id = NULL, values = NULL, group = 5) {

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  col_name <- rlang::as_label(qvalues)

  # get the columns type
  type <- sapply(data, class)[[col_name]]

  # if the class of the values column is numeric or integer
  if (type == "numeric"|type == "integer") {
    return(
      data |>
        group_by(!!qid) |>
        arrange(!!qid) |>
        mutate(time = row_number(),
                      time = time + floor(runif(1, 1, 100)),
                      rank = as.integer(rank(-!!qvalues)),
                      percentile = rank(-!!qvalues)/length(!!qvalues),
                      # using the non-standard evaluation to automatically assign the position on the plot
                      pos = case_when(!!!pos_case(group)),
                      pos = as.numeric(pos)) |>
        select(-c(rank, percentile)) |>
        ungroup()
    )
  }

  # if the class of the values column is factor
  if (type == "factor") {
    return(
      data |>
        group_by(!!qid) |>
        arrange(!!qid) |>
        mutate(time = row_number(),
                      time = time + floor(runif(1, 10, 100)),
                      pos = as.numeric(!!qvalues))
    )
  }

}
