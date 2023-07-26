#' @import rlang

pos_case <- function(group = 5) {

  n <- length(group)
  pos_list <- list()
  rev_vec <- rev(group)


  # if the user supplied the vector
  if (n > 1) {
    for(i in seq_along(group)) {
      q <- 1 - (i * (1/n))
      case <- paste0(".data$percentile > ", q, " ~ ", rev_vec[i])

      pos_list[[i]] <- case
    }

    list <- rlang::parse_exprs(paste(pos_list))

    return(list)
  }

  # if the user supplied the number of group
  if (n == 1) {
    for(i in 1:group) {
      q <- 1 - (i * (1/group))
      case <- paste0(".data$percentile > ", round(q, 2), " ~ ", abs(i - group))

      pos_list[[i]] <- case
    }

    list <- parse_exprs(paste(pos_list))

    return(list)
  }
}

