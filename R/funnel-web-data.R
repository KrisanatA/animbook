#' Funnel web spider plot data
#'
#' This function performs data manipulation for facetting.
#'
#' @param object An animbook object
#'
#' @return A modified animbook object
#'
#' @details The function takes the animbook object and manipulates the data into the
#' format where it can be further facetting.
#'
#' @keywords internal

funnel_web_spider_data <- function(data) {


  # check -------------------------------------------------------------------

  stopifnot("Please use the prep function to convert the data into categorized format" =
              any("categorized" %in% class(data)))

  class(data) <- c("tbl_df", "tbl", "data.frame")

  x <- unique(data$time)


  # Change the data format --------------------------------------------------

  start <- data |>
    dplyr::filter(time > min(time)) |>
    dplyr::group_by(id) |>
    dplyr::mutate(facet = dplyr::row_number())

  end <- data |>
    dplyr::filter(time < max(time)) |>
    dplyr::group_by(id) |>
    dplyr::mutate(facet = dplyr::row_number())

  full <- rbind(start, end) |>
    dplyr::arrange(id, time)

  object <- list(data = full,
                 xbreaks = sort(x))

  return(object)
}
