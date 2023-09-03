#'Kangaroo plot data
#'
#'This function performs data manipulation and formatting tasks
#'from the original object with additional data components for labeling and shading.
#'
#'@param object An animbook object
#'
#'@return A modified animbook object with addition data components
#'
#'@details The function takes the animbook object and create a new label data and
#'shading data used for the plot then appends them to the original object.
#'
#'@keywords internal
#'
#'@export

kangaroo_data <- function(object) {

  data <- object[["data"]]

  x <- unique(data$time)

  y <- sort(unique(data$qtile), decreasing = TRUE)

  gap <- object[["settings"]]$gap

  unique_qtiles <- unique(data$qtile)

# Create label data -------------------------------------------------------

  label_data <- tibble::tibble(
    x = min(x) - (2 * gap),
    y = y,
    label = object[["settings"]]$label
  )


# Create shading data -----------------------------------------------------

  shade_data <- tibble::tibble(
    id = y,
    xmin = rep(min(x) - gap, length(y)),
    xmax = rep(max(x) + gap, length(y)),
    ymin = y - 0.25,
    ymax = y + 0.25
  )


  kangaroo <- append(object, list(label_data, shade_data))

  names(kangaroo) <- c("data", "settings", "label_data", "shade_data")

  return(kangaroo)

}





#'Wallaby plot data
#'
#'This function performs data manipulation and formatting tasks
#'from the original object with additional data components for labeling and shading.
#'
#'@param object An animbook object
#'
#'@return A modified animbook object with addition data components
#'
#'@details The function takes the animbook object then subset the data based on the users.
#'Additionaly, it create a label data and shading data for the [anim_plot()] function. All
#'of this then replaced the original data and append new data to the object.
#'
#'@keywords internal
#'
#'@export

wallaby_data <- function(object,
                         subset = "top") {

  subset_choice <- c("top", "bottom")

  stopifnot("Subset argument can only be either top or bottom" =
              subset %in% subset_choice)

  data <- object[["data"]]

  unique_qtiles <- unique(data$qtile)

  y <- sort(unique(data$qtile), decreasing = TRUE)

# Subset the data ---------------------------------------------------------

  if (subset == "top") {
    subset <- max(data$qtile)

    label_left <- object[["settings"]]$label[1]
  }

  if (subset == "bottom") {
    subset <- min(data$qtile)

    label_left <- object[["settings"]]$label[length(unique_qtiles)]
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

  object[["settings"]]$xbreaks <- c(0, 1)


# Create label data -------------------------------------------------------

  gap <- 0.1 * length(unique(subset_data$time) - 1)

  object[["settings"]]$gap <- gap

  left <- tibble::tibble(
    x = min(subset_data$time) - gap,
    y = max(y),
    label = label_left
  )

  right <- tibble::tibble(
    x = max(subset_data$time) + gap,
    y = y,
    label = object[["settings"]]$label
  )

  label_data <- list(left, right)


# Create shading data -----------------------------------------------------

  max_qtile <- max(unique_qtiles)
  min_qtile <- min(unique_qtiles)

  point0 <- tibble::tibble(
    id = rep(unique_qtiles, each = 2),
    x = 0,
    y = rep(c(max_qtile + 0.25, max_qtile - 0.25), length(unique_qtiles))
  )

  point1 <- tibble::tibble(
    id = rep(unique_qtiles, 2),
    x = 1,
    y = c(unique_qtiles - 0.25, unique_qtiles + 0.25)
  )

  shade_data <- rbind(point0, point1)


  wallaby <- append(object, list(label_data, shade_data))

  names(wallaby) <- c("data", "settings", "label_data", "shade_data")

  names(wallaby$label_data) <- c("left", "right")

  return(wallaby)

}





#'Funnel web spider plot data
#'
#'This function performs data manipulation for facetting.
#'
#'@param object An animbook object
#'
#'@return A modified animbook object
#'
#'@details The function takes the animbook object and manipulate the data into the
#'format where it can be further facetting.
#'
#'@keywords internal
#'
#'@export

funnel_web_spider_data <- function(object) {

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


