#'@importFrom ggplot2 element_blank
#'
#'@export

wallaby_plot <- function(object,
                         palette = RColorBrewer::brewer.pal(9, "Set1"),
                         rendering = "ggplot",
                         subset = "top",
                         relation = "one_many",
                         ...) {

  col_val <- palette

  rendering_choice <- c("ggplot", "plotly")

  stopifnot("Please use the anim_prep function to converted data into animbook class object" =
              class(object) == "animbook",
            "The rendering argument can only be either ggplot or plotly" =
              rendering %in% rendering_choice)


# ... arguments -----------------------------------------------------------

  args <- list(...)

  # height settings for geom_jitter
  height <- 0.3

  if (!is.null(args[["height"]])) {
    height <- args[["height"]]
  }

  # width settings for geom_jitter
  width <- 0

  if (!is.null(args[["width"]])) {
    width <- args[["width"]]
  }

  # alpha settings for paths shading
  alpha <- 0.1

  if (!is.null(args[["alpha"]])) {
    alpha <- args[["alpha"]]
  }

  args_accepted <- c("height", "width", "alpha")

  if (any(!(names(args) %in% args_accepted))) {
    warning(paste0("The following arguments are not supported: ",
                   paste0(names(args)[!(names(args) %in% args_accepted)], collapse = ", ")))
  }


# format data -------------------------------------------------------------

  object <- wallaby_data(object, subset = subset, relation = relation)


# variable main aes() -----------------------------------------------------

  if (rendering == "ggplot") {

    if ("color" %in% colnames(object[["data"]])) {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        group = quote(id),
        color = quote(color)
      )
    }

    else {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        group = quote(id)
      )
    }
  }

  if (rendering == "plotly") {

    if ("color" %in% colnames(object[["data"]])) {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        color = quote(color),
        ids = quote(id),
        frame = quote(frame)
      )
    }

    else {
      aes_list <- list(
        x = quote(x),
        y = quote(y),
        ids = quote(id),
        frame = quote(frame)
      )
    }
  }


# draw plot ---------------------------------------------------------------

  # the data point
  australia <- ggplot2::ggplot() +
    ggplot2::geom_point(data = object[["data"]],
                         mapping = ggplot2::aes(!!!aes_list)
                         ) |>
    suppressWarnings()

  # the shaded area + label
  australia <- australia +
    ggplot2::geom_polygon(data = object[["shade_data"]],
                          ggplot2::aes(x = x,
                                       y = y,
                                       group = id,
                                       fill = as.factor(id)),
                          alpha = alpha) +
    ggplot2::geom_text(data = object[["label_data"]]$right,
                       ggplot2::aes(x = x,
                                    y = y,
                                    label = label)) +
    ggplot2::geom_text(data = object[["label_data"]]$left,
                       ggplot2::aes(x = x,
                                    y = y,
                                    label = label)) |>
    suppressWarnings()

  # plot settings
  anim <- australia +
    ggplot2::scale_x_continuous(breaks = object[["settings"]]$xbreaks) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(panel.background = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.line.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.text.x = element_blank(),
                   legend.position = "bottom",
                   legend.title = element_blank()) +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_fill_manual(values = col_val) +
    ggplot2::scale_colour_manual(values = col_val)

  return(anim)

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

wallaby_data <- function(object,
                         subset = "top",
                         relation = "one_many",
                         height = 0.6) {

# subset choice -----------------------------------------------------------

  choice <- as.character(object[["settings"]]$label)

  subset_choice <- c("top", "bottom", choice)

  if (!(subset %in% subset_choice)) {
    stop(paste0("subset can only be the following: ",
                paste0(subset_choice, collapse = ", ")))
  }


# relation choice ---------------------------------------------------------

  relation_choice <- c("one_many", "many_one")

  stopifnot("relation can only be the following: one_many, many_one" =
              relation %in% relation_choice)


# data --------------------------------------------------------------------

  data <- object[["data"]]

  unique_qtiles <- as.numeric(sort(unique(data$qtile), decreasing = TRUE))

  y_label <- object[["settings"]]$label

  match_table <- tibble::tibble(unique_qtiles = unique_qtiles,
                                y_label = y_label)


# subset argument ---------------------------------------------------------

  if (subset == "top") {
    subset_qtile <- max(unique_qtiles)

    label_subset <- y_label[1]
  }

  if (subset == "bottom") {
    subset_qtile <- min(unique_qtiles)

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

    y_right <- unique_qtiles

    label_left <- label_subset

    label_right <- y_label

    position <- "left"
  }

  if (relation == "many_one") {
    time_subset <- max(data$time)

    y_left <- unique_qtiles

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

  object[["settings"]]$xbreaks <- c(0, 1)


# create label data -------------------------------------------------------

  gap <- 0.1 * length(unique(filter_data$time) - 1)

  object[["settings"]]$gap <- gap

  left <- tibble::tibble(
    x = min(filter_data$time) - gap,
    y = y_left,
    label = label_left
  )

  right <- tibble::tibble(
    x = max(filter_data$time) + gap,
    y = y_right,
    label = label_right
  )

  label_data <- list(left, right)


# calculate proportion ----------------------------------------------------

  if (relation == "one_many") {
    prop_time <- max(filter_data$time)

    x_point <- min(filter_data$time)
  }

  if (relation == "many_one") {
    prop_time <- min(filter_data$time)

    x_point <- max(filter_data$time)
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

  shade_data <- sankey_shade(initial = initial,
                             proportion = prop,
                             y = shading_y,
                             position = position)


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
    new_data <- filter_data |>
      dplyr::select(-frame) |>
      tidyr::pivot_wider(names_from = time,
                         values_from = qtile) |>
      dplyr::left_join(lookup_newpoint,
                       by = c(`1` = "qtile")) |>
      dplyr::select(-`0`) |>
      dplyr::rename(`0` = point) |>
      tidyr::pivot_longer(-c(id, color),
                          names_to = "time",
                          values_to = "qtile") |>
      dplyr::mutate(time = as.numeric(time))
  }

  if (relation == "many_one") {
    new_data <- filter_data |>
      dplyr::select(-frame) |>
      tidyr::pivot_wider(names_from = time,
                         values_from = qtile) |>
      dplyr::left_join(lookup_newpoint,
                       by = c(`0` = "qtile")) |>
      dplyr::select(-`1`) |>
      dplyr::rename(`1` = point) |>
      tidyr::pivot_longer(-c(id, color),
                          names_to = "time",
                          values_to = "qtile") |>
      dplyr::mutate(time = as.numeric(time))
  }


# create sigmoid path -----------------------------------------------------

  path <- new_data |>
    tidyr::pivot_wider(id_cols = id,
                       names_from = time,
                       values_from = qtile) |>
    dplyr::mutate(xstart = 0, xend = 1) |>
    dplyr::group_by(id) |>
    dplyr::mutate(path = purrr::map(.data, ~sigmoid(xstart, xend,
                                                    `0`, `1`,
                                                    scale = 10, n = 40))) |>
    dplyr::select(id, path) |>
    tidyr::unnest(cols = path)


# calculate frame ---------------------------------------------------------

  if (object[["settings"]]$time_dependent == TRUE) {
    data <- path |>
      dplyr::mutate(
        frame = dplyr::row_number()
      )
  }

  if (object[["settings"]]$time_dependent == FALSE) {
    data <- path |>
      dplyr::mutate(
        frame = dplyr::row_number(),
        frame = frame + floor(runif(1,
                                    object[["settings"]]$runif_min,
                                    object[["settings"]]$runif_max))
      )
  }


# join the missing information --------------------------------------------

  information <- new_data |>
    dplyr::select(id, color, qtile) |>
    dplyr::left_join(prop_table, by = "qtile") |>
    dplyr::select(id, color, prop) |>
    dplyr::distinct()

  wallaby_data <- data |>
    dplyr::left_join(information,
                     by = "id") |>
    dplyr::mutate(y = y + runif(1, -prop * (height/2), prop * (height/2)))


# output ------------------------------------------------------------------

  object[["data"]] <- wallaby_data

  wallaby <- append(object, list(label_data, shade_data))

  names(wallaby) <- c("data", "settings", "label_data", "shade_data")

  names(wallaby$label_data) <- c("left", "right")

  return(wallaby)

}













