kangaroo_data <- function(object) {

  data <- object[["data"]]

  x <- unique(time)

  y <- sort(unique(data$qtile), decreasing = TRUE)

  gap <- object[["settings"]]$gap

  unique_qtiles <- unique(data$qtile)

# Create label data -------------------------------------------------------

  label_data <- data.frame(
    x = min(x) - (2 * gap),
    y = sort(unique_qtiles, decreasing = TRUE),
    label = data[["settings"]]$label
  )


# Create shading data -----------------------------------------------------

  shade_data <- tibble::tibble(
    id = y,
    xmin = rep(min(x) - gap, length(y)),
    xmax = rep(max(x) + gap, length(y)),
    ymin = y - 0.25,
    ymax = y + 0.25
  )


  kangaroo <- append(object, c(label_data, shade_data))

  return(kangaroo)

}

















wallyby_data <- function(object,
                         subset = "top") {

  subset_choice <- c("top", "bottom")

  stopifnot("Subset argument can only be either top or bottom" =
              subset %in% subset_choice)

  data <- object[["data"]]

  unique_qtiles <- unique(data$qtile)


# Subset the data ---------------------------------------------------------

  if (subset == "top") {
    subset <- max(data$qtile)
  }

  if (subset == "bottom") {
    subset <- min(data$qtile)
  }

  subset_id <- data |>
    dplyr::filter(time == min(time),
           qtile == subset) |>
    dplyr::pull(id)

  subset_data <- data |>
    dplyr::filter(id %in% subset_id) |>
    dplyr::mutate(time = dplyr::case_when(
      time == min(time) ~ 0,
      time == max(time) ~ 1
    )) |>
    na.omit()

  object[["data"]] <- subset_data


# Create label data -------------------------------------------------------

  left_label <- data.frame(
    id = sort(unique_qtiles, decreasing = TRUE),
    x = min(subset_data$time),
    y = max(sort(unique_qtiles))
  )

  right_label <- data.frame(
    id = sort(unique_qtiles, decreasing = TRUE),
    x = max(subset_data$time),
    y = sort(unique(data$qitle), decreasing = TRUE)
  )

  label_data <- list(left_label, right_label)


# Create shading data -----------------------------------------------------

  max_qtile <- max(unique_qtiles)
  min_qtile <- min(unique_qtiles)

  point0 <- data.frame(
    id = rep(unique_qtiles, each = 2),
    x = 0,
    y = rep(c(max_qtile + 0.1, max_qtile - 0.1), length(unique_qtiles))
  )

  point1 <- data.frame(
    id = rep(unique_qtiles, 2),
    x = 1,
    y = rep(c(unique_qtiles - 0.1, unique_qtiles + 0.1), 2)
  )

  shade_data <- rbind(point0, point1)


  wallaby <- append(object, c(label_data, shade_data))

  return(wallaby)

}













funnel_web_spider <- function(object) {

  data <- object[["data"]]


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


  object[["data"]] <- full

  return(object)
}





