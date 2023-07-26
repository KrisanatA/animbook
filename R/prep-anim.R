#' @import dplyr

prep_anim <- function(df, id = NULL, values = NULL, n_group = 5) {

  qid <- enquo(id)
  qvalues <- enquo(values)
  col_name <- as_label(qvalues)

  # get the columns type
  type <- sapply(df, class)[[col_name]]

  # if the class of the values column is numeric or integer
  if (type == "numeric"|type == "integer") {
    return(
      df |>
        group_by(!!qid) |>
        arrange(!!qid) |>
        mutate(time = row_number(),
                      time = time + floor(runif(1, 1, 100)),
                      rank = as.integer(rank(-!!qvalues)),
                      percentile = rank(-!!qvalues)/length(!!qvalues),
                      pos = case_when(!!!pos_case(n_group)),
                      pos = as.numeric(pos)) |>
        select(-c(rank, percentile)) |>
        ungroup()
    )
  }

  # if the class of the values column is factor
  if (type == "factor") {
    return(
      df |>
        group_by(!!qid) |>
        arrange(!!qid) |>
        mutate(time = row_number(),
                      time = time + floor(runif(1, 10, 100)),
                      pos = as.numeric(!!qvalues))
    )
  }

}
