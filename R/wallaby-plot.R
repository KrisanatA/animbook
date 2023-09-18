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
  height <- 0.2

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
#'
#'@export
#'

wallaby_data <- function(object,
                         subset = "top",
                         relation = "one_many") {

  choice <- as.character(object[["settings"]]$label)

  subset_choice <- c("top", "bottom", choice)

  relation_choice <- c("one_many", "many_one")

  # check subset argument
  if (!(subset %in% subset_choice)) {
    stop(paste0("subset can only be the following: ",
                paste0(subset_choice, collapse = ", ")))
  }

  # check relation argument
  stopifnot("relation can only be the following: one_many, many_one" =
              relation %in% relation_choice)

  data <- object[["data"]]

  unique_qtiles <- unique(data$qtile)

  y <- sort(unique(data$qtile), decreasing = TRUE)


# subset argument ---------------------------------------------------------

  if (subset == "top") {
    subset <- max(data$qtile)

    label_pos <- object[["settings"]]$label[1]
  }

  if (subset == "bottom") {
    subset <- min(data$qtile)

    label_pos <- object[["settings"]]$label[length(unique_qtiles)]
  }

  if (subset %in% choice) {
    subset_pos <- which(rev(object[["settings"]]$label) == subset)

    subset <- as.numeric(rev(object[["settings"]]$label)[subset_pos])

    label_pos <- rev(object[["settings"]]$label)[subset_pos]
  }


# relation argument -------------------------------------------------------

  if (relation == "one_many") {
    time_subset <- min(data$time)

    y_left <- subset

    y_right <- y

    label_left <- label_pos

    label_right <- object[["settings"]]$label

    position <- "left"
  }

  if (relation == "many_one") {
    time_subset <- max(data$time)

    y_left <- y

    y_right <- subset

    label_left <- object[["settings"]]$label

    label_right <- label_pos

    position <- "right"
  }


# data manipulation -------------------------------------------------------

  subset_id <- data |>
    dplyr::filter(time == time_subset,
                  qtile == subset) |>
    dplyr::pull(id)

  subset_data <- data |>
    dplyr::filter(id %in% subset_id) |>
    dplyr::mutate(time = dplyr::case_when(time == min(time) ~ 0,
                                          time == max(time) ~ 1)) |>
    na.omit()

  object[["settings"]]$xbreaks <- c(0, 1)


# create label data -------------------------------------------------------

  gap <- 0.1 * length(unique(subset_data$time) - 1)

  object[["settings"]]$gap <- gap

  left <- tibble::tibble(
    x = min(subset_data$time) - gap,
    y = y_left,
    label = label_left
  )

  right <- tibble::tibble(
    x = max(subset_data$time) + gap,
    y = y_right,
    label = label_right
  )

  label_data <- list(left, right)


# create shading data -----------------------------------------------------

  prop_table <- subset_data |>
    dplyr::filter(time == 1) |>
    dplyr::count(qtile) |>
    dplyr::mutate(prop = n/sum(n),
                  prop = ifelse(prop < 0.1, 0.1, prop))

  subset_data <- prop_table |>
    dplyr::select(qtile, prop) |>
    dplyr::right_join(subset_data |> filter(time == 1),
                      by = "qtile") |>
    dplyr::select(id, prop) |>
    dplyr::right_join(subset_data,
                      by = "id")

  prop <- dplyr::pull(prop_table, prop)

  initial <- subset + sum(prop)/2

  y <- sort(unique(subset_data$qtile)) + (prop/2)

  shade_data <- sankey_shade(initial = initial,
                             proportion = rev(prop),
                             gap = gap,
                             y = y,
                             position = position)

  starting_point <- shade_data |>
    dplyr::group_by(id) |>
    dplyr::filter(x == min(x)) |>
    dplyr::summarise(point = mean(y)) |>
    dplyr::arrange(point) |>
    dplyr::select(point)

  qtile <- subset_data |>
    dplyr::filter(time == 1) |>
    dplyr::distinct(qtile) |>
    dplyr::arrange(qtile)

  lookup_table <- tibble::tibble(cbind(starting_point, qtile))

  subset_data <- subset_data |>
    dplyr::arrange(id, time) |>
    dplyr::left_join(lookup_table, by = "qtile") |>
    dplyr::mutate(point = dplyr::lead(point),
                  qtile = ifelse(time == 0, point, qtile)) |>
    dplyr::select(-point)

  path <- subset_data |>
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

  information <- subset_data |>
    dplyr::select(id, color, prop) |>
    dplyr::distinct()

  wallaby_data <- data |>
    dplyr::left_join(information,
              by = "id") |>
    dplyr::mutate(y = y + runif(1, -prop/3, prop/3))

  object[["data"]] <- wallaby_data

  wallaby <- append(object, list(label_data, shade_data))

  names(wallaby) <- c("data", "settings", "label_data", "shade_data")

  names(wallaby$label_data) <- c("left", "right")

  return(wallaby)

}













