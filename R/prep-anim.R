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
        dplyr::group_by(!!qid) |>
        dplyr::arrange(!!qid) |>
        dplyr::mutate(time = row_number(),
                      time = time + floor(runif(1, 10, 100)),
                      rank = as.integer(rank(-!!qvalues)),
                      percentile = rank(-!!qvalues)/length(!!qvalues),
                      pos = case_when(!!!pos_case(n_group)),
                      pos = as.factor(pos)) |>
        dplyr::select(-c(rank, percentile)) |>
        dplyr::ungroup()
    )
  }

  # if the class of the values column is factor
  if (type == "factor") {
    return(
      df |>
        dplyr::group_by(!!qid) |>
        dplyr::arrange(!!qid) |>
        dplyr::mutate(time = row_number(),
                      time = time + floor(runif(1, 10, 100)),
                      pos = !!qvalues)
    )
  }

}
