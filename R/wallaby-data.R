#' Wallaby plot data
#'
#' This function performs data manipulation and path interpolation of the
#' categorized data. This includes additional data components for labeling and
#' shading.
#'
#' @param data The categorized data.
#' @param subset A character string specifying the variable used for subsetting
#' the data. The "top" and "bottom" strings can also be used in this argument.
#' @param relation The choice of relationship for the values to display on the
#' plot, either "one_many" or "many_one."
#' @param time_dependent Logical. Should the visualizations be time-dependent?
#' The default is FALSE.
#' @param height The proportion the point takes in the shaded area.
#' @param width The number that controls the runif_max to specify how far apart
#' each point is.
#' @param total_point The number of points the users want for the wallaby plot.
#' The default is NULL, where the number of the point is equal to the original number
#' of points.
#'
#' @return An object contained the modified data with additional data components.
#'
#' @details The function takes the categorized object and interpolates the path
#' for each observation. Additionally, the label and shading data are created.
#'
#' @keywords internal
#'
#' @importFrom dplyr .data

wallaby_data <- function(data,
                         subset = "top",
                         relation = "one_many",
                         time_dependent = FALSE,
                         height = 0.6,
                         width = 50L,
                         total_point = NULL) {


  # check -------------------------------------------------------------------

  stopifnot("height argument only accepted proportion between 0 and 1" =
              dplyr::between(height, 0, 1),
            "Please use the prep function to convert the data into a categorized format" =
              any("categorized" %in% class(data)))

  class(data) <- c("tbl_df", "tbl", "data.frame")

  x <- unique(data$time)

  y <- sort(unique(data$qtile), decreasing = TRUE)

  label <- data |>
    dplyr::distinct(qtile, label) |>
    dplyr::arrange(dplyr::desc(qtile)) |>
    dplyr::pull(label)


  # gap settings ------------------------------------------------------------

  gap <- 0.1


  # subset_choice -----------------------------------------------------------

  choice <- as.character(label)

  subset_choice <- c("top", "bottom", choice)

  if (!(subset %in% subset_choice)) {
    stop(paste0("subset can only be the following: ",
                paste0(subset_choice, collapse = ", ")))
  }


  # relation choice ---------------------------------------------------------

  relation_choice <- c("one_many", "many_one")

  stopifnot("relation argument can only be either one_many or many_one" =
              relation %in% relation_choice)

  y_label <- label

  match_table <- tibble::tibble(unique_qtiles = as.numeric(y),
                                y_label = y_label)


  # subset argument ---------------------------------------------------------

  if (subset == "top") {
    subset_qtile <- max(as.numeric(y))

    label_subset <- y_label[1]
  }

  if (subset == "bottom") {
    subset_qtile <- min(as.numeric(y))

    label_subset <- y_label[length(y_label)]
  }

  if (subset %in% choice) {
    subset_qtile <- match_table |>
      dplyr::filter(y_label == subset) |>
      dplyr::pull(unique_qtiles)

    label_subset <- subset
  }


  # relation argument -------------------------------------------------------

  if (relation == "one_many") {
    time_subset <- min(data$time)

    y_left <- subset_qtile

    y_right <- as.numeric(y)

    label_left <- label_subset

    label_right <- y_label

    position <- "left"
  }

  if (relation == "many_one") {
    time_subset <- max(data$time)

    y_left <- as.numeric(y)

    y_right <- subset_qtile

    label_left <- y_label

    label_right <- label_subset

    position <- "right"
  }


  # filter data -------------------------------------------------------------

  subset_id <- data |>
    dplyr::filter(time == time_subset,
                  qtile == subset_qtile) |>
    dplyr::pull(id)

  filter_data <- data |>
    dplyr::filter(id %in% subset_id) |>
    dplyr::mutate(time = dplyr::case_when(time == min(time) ~ 0,
                                          time == max(time) ~ 1)) |>
    stats::na.omit()


  # labels data -------------------------------------------------------------

  left <- tibble::tibble(
    x = 0 - gap,
    y = y_left,
    label = label_left
  )

  right <- tibble::tibble(
    x = 1 + gap,
    y = y_right,
    label = label_right
  )


  # calculate proportion ----------------------------------------------------

  if (relation == "one_many") {
    prop_time <- 1

    x_point <- 0
  }

  if (relation == "many_one") {
    prop_time <- 0

    x_point <- 1
  }

  prop_table <- filter_data |>
    dplyr::filter(time == prop_time) |>
    dplyr::count(qtile) |>
    dplyr::mutate(prop = n/sum(n),
                  prop = ifelse(prop < 0.1, 0.1, prop)) |>
    dplyr::arrange(dplyr::desc(qtile))

  prop <- dplyr::pull(prop_table, prop)

  # create shading data -----------------------------------------------------

  initial <- subset_qtile + sum(prop)/2

  shading_y <- dplyr::pull(prop_table, qtile) + (prop/2)

  shade_data <- proportional_shade(initial = initial,
                             proportion = prop,
                             y = shading_y,
                             position = position)

  # interpolate data --------------------------------------------------------

  if ("group" %in% colnames(data)) {
    count_list <- list(
      quote(qtile),
      quote(group)
    )
  }

  else {
    count_list <- list(
      quote(qtile)
    )
  }

  if (is.null(total_point)) {
    total_point <- nrow(filter_data)
  }

  if (!is.null(total_point) & total_point <= nrow(filter_data)) {
    total_point <- nrow(filter_data)
  }

  if (!is.null(total_point) & total_point > nrow(filter_data)) {
    total_point <- total_point
  }

  count_data <- filter_data |>
    dplyr::filter(time == prop_time) |>
    dplyr::count(!!!count_list) |>
    dplyr::arrange(dplyr::desc(n))

  n_point <- count_data |>
    dplyr::mutate(prop = n/sum(n),
                  n_point = ceiling(total_point * prop)) |>
    dplyr::pull(n_point)

  interpolate_data <- count_data |>
    dplyr::select(-n) |>
    dplyr::slice(rep(1:dplyr::n(), times = n_point)) |>
    dplyr::mutate(id = dplyr::row_number())


  # change the start or end point of the subset data ------------------------

  new_point <- shade_data |>
    dplyr::group_by(id) |>
    dplyr::filter(x == x_point) |>
    dplyr::summarise(point = mean(y)) |>
    dplyr::arrange(point) |>
    dplyr::select(point)

  qtile <- filter_data |>
    dplyr::filter(time == prop_time) |>
    dplyr::distinct(qtile) |>
    dplyr::arrange(qtile)

  lookup_newpoint <- tibble::tibble(cbind(qtile, new_point))

  if (relation == "one_many") {
    new_data <- interpolate_data |>
      dplyr::left_join(lookup_newpoint,
                       by = "qtile") |>
      dplyr::rename(`0` = point,
                    `1` = qtile) |>
      tidyr::pivot_longer(c(`0`, `1`),
                          names_to = "time",
                          values_to = "qtile") |>
      dplyr::mutate(time = as.numeric(time))
  }

  if (relation == "many_one") {
    new_data <- interpolate_data |>
      dplyr::left_join(lookup_newpoint,
                       by = "qtile") |>
      dplyr::rename(`1` = point,
                    `0` = qtile) |>
      tidyr::pivot_longer(c(`0`, `1`),
                          names_to = "time",
                          values_to = "qtile") |>
      dplyr::mutate(time = as.numeric(time))
  }


  # create path -------------------------------------------------------------

  path <- new_data |>
    tidyr::pivot_wider(id_cols = id,
                       names_from = time,
                       values_from = qtile) |>
    dplyr::mutate(xstart = 0, xend = 1) |>
    dplyr::group_by(id) |>
    dplyr::mutate(path = purrr::map(.data, ~sine(xstart, xend,
                                                 `0`, `1`,
                                                 n = 40))) |>
    dplyr::select(id, path) |>
    tidyr::unnest(cols = path)


  # calculate frame ---------------------------------------------------------

  # check time_dependent argument
  stopifnot("time_dependent argument only accepted logical values" =
              is.logical(time_dependent))

  if (time_dependent == TRUE) {
    data <- path |>
      dplyr::mutate(
        frame = dplyr::row_number()
      )
  }

  if (time_dependent == FALSE) {
    data <- path |>
      dplyr::mutate(
        frame = dplyr::row_number(),
        frame = frame + floor(stats::runif(1,
                                           1,
                                           width))
      )
  }

  # join the missing information --------------------------------------------

  information <- new_data |>
    dplyr::select(-time) |>
    dplyr::left_join(prop_table, by = "qtile") |>
    dplyr::select(-c(qtile, n)) |>
    dplyr::distinct() |>
    stats::na.omit()

  wallaby_data <- data |>
    dplyr::left_join(information,
                     by = "id") |>
    dplyr::mutate(y = y + stats::runif(1, -prop * (height/2), prop * (height/2)))


  # output ------------------------------------------------------------------

  object <- list(data = wallaby_data,
                 left_data = left,
                 right_data = right,
                 shade_data = shade_data,
                 xbreaks = c(0, 1))

  return(object)

}
