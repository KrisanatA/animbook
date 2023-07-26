#' @import rlang

pos_case <- function(n_group = 5) {

  n <- length(n_group)

  pos_list <- list()

  # if (n > 1) {
  #   for(i in 1:n) {
  #     q <- 1 - (i * (1/n))
  #     case <- paste0(".data$percentile > ", q, " ~ ", n_group[i])
  #
  #     pos_list[[i]] <- case
  #   }
  #
  #   list <- rlang::parse_exprs(paste(pos_list))
  #
  #   return(list)
  # }

  # if the user supplied the number of group
  if (n == 1) {
    for(i in 1:n_group) {
      q <- 1 - (i * (1/n_group))
      case <- paste0(".data$percentile > ", round(q, 2), " ~ ", abs(i - n_group))

      pos_list[[i]] <- case
    }

    list <- parse_exprs(paste(pos_list))

    return(list)
  }
}

