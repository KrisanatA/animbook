prep_cat <- function(data,
                     id = NULL,
                     values = NULL,
                     time = NULL,
                     time_dependent = FALSE) {

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)

  type <- sapply(data, class)[[rlang::as_label(qvalues)]]

  stopifnot("This function only accepted character values" = type == "character")

}
