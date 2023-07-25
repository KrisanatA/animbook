prep_anim <- function(df, id = NULL, values = NULL, n_group = 5) {

  qid <- enquo(id)
  qvalues <- enquo(values)
  col_name <- as_label(qvalues)

  pos_list <- list()

  # get the columns type
  type <- sapply(df, class)[[col_name]]

  # automatically assign the pos to the values if the class of the column is numeric or integer
  for(i in 1:n_group) {
    n <- n_group
    q <- 1 - (i * (1/n))
    case <- paste0(".data$percentile > ", q, " ~ ", abs(i - 6))

    pos_list[[i]] <- case
    }

  list <- rlang::parse_exprs(paste(pos_list, sep = ", "))

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
                      pos = case_when(!!!list),
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
