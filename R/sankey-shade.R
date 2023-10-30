#' Sankey shade data
#'
#' This function creates a shaded Sankey using the specified parameters.
#'
#' @param initial A numerical of initial values for the Sankey shade.
#' @param proportion A vector of proportions corresponding to each group.
#' @param y A numeric vector specifying the vertical position of the Sankey diagram.
#' @param position A character string specifying the side of the Sankey diagram.
#' This needs to be either "left" (default) or "right".
#'
#' @return A data frame of the shaded Sankey diagram.
#'
#' @examples
#' data <- sankey_shade(4, c(0.4, 0.3, 0.2, 0.1), c(4, 3, 2, 1))
#'
#' ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y, group = id, fill = as.factor(id))) +
#'   ggplot2::geom_polygon(alpha = 0.1)
#'
#' @keywords internal
#'
#' @export

sankey_shade <- function(initial = NULL,
                         proportion = NULL,
                         y = NULL,
                         position = "left") {

  position_choice <- c("left", "right")

  stopifnot("The position argument can only be left or right" =
              position %in% position_choice)


# initial setup -----------------------------------------------------------

  initial <- initial

  prop <- proportion

  top <- vector()

  bottom <- vector()

  y <- sort(y, decreasing = TRUE)


# calculate top and bottom point on the left hand side --------------------

  for (i in 1:length(y)) {
    top[i] <- initial

    bottom[i] <- initial - prop[i]

    initial <- initial - prop[i]
  }


# ystart and yend ---------------------------------------------------------

  if (position == "left") {
    ystart <- c(top, bottom)

    yend <- c(y, y - prop)
  }

  if (position == "right") {
    ystart <- c(y, y - prop)

    yend <- c(top, bottom)
  }


# left point data ---------------------------------------------------------

  left <- tibble::tibble(ystart = ystart,
                         xstart = 0) |>
    dplyr::arrange(dplyr::desc(ystart)) |>
    dplyr::mutate(id = dplyr::row_number())


# right point data --------------------------------------------------------

  right <- tibble::tibble(yend = yend,
                          xend = 1) |>
    dplyr::arrange(dplyr::desc(yend)) |>
    dplyr::mutate(id = dplyr::row_number())


# full point data ---------------------------------------------------------

  full <- left |>
    dplyr::left_join(right,
                     by = "id")


# map the point with sine path -----------------------------------------

  map <- purrr::map_dfr(seq_len(nrow(full)),
                        ~ sine(as.numeric(full[.x, 2]), as.numeric(full[.x, 5]),
                                  as.numeric(full[.x, 1]), as.numeric(full[.x, 4]),
                                  n = 100) |>
                          dplyr::mutate(id = .x))

  split_map <- split(map, map$id)


# arrange it for polygon --------------------------------------------------

  for (i in seq(2, length(split_map), by = 2)) {
    split_map[[i]] <- split_map[[i]] |>
      dplyr::arrange(dplyr::desc(x))

    split_map[[i-1]]$id <- i - 1
    split_map[[i]]$id <- i - 1
  }

  shade_data <- do.call("rbind", split_map)


  return(shade_data)

}
