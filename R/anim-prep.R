anim_prep <- function(data,
                      id = NULL,
                      values = NULL,
                      time = NULL,
                      label = NULL,
                      ngroup = 5L,
                      breaks = NULL,
                      group = NULL,
                      time_dependent = TRUE,
                      scaling = "rank",
                      runif_min = 1,
                      runif_max = 50) {


# enquo -------------------------------------------------------------------

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)
  qgroup <- rlang::enquo(group)


# check column class ------------------------------------------------------

  type <- sapply(data, class)

  stopifnot("The id column need to be factor variable" =
              type[[rlang::as_label(id)]] == "factor",
            "The values column need to be numeric variable" =
              type[[rlang::as_label(values)]] == "numeric",
            "The time column need to be integer variable" =
              type[[rlang::as_label(time)]] == "integer")


# scaling choice ----------------------------------------------------------

  scaling_choice <- c("rank", "absolute")

  stopifnot("The scaling can either be rank or absolute" =
              scaling %in% c(scaling_choice))


# assign the frames -------------------------------------------------------

  if (time_dependent == FALSE) {
    data_frame <- data |>
      arrange(!!qid, !!qtime) |>
      group_by(!!qid) |>
      mutate(
        frame = dplyr::row_number(),
        frame = frame + floor(runif(1, runif_min, runif_max))
      ) |>
      ungroup()
  }

  if (time_dependent == TRUE) {
    data_frame <- data |>
      arrange(!!qid, !!qtime) |>
      group_by(!!qid) |>
      mutate(
        frame = dplyr::row_number()
      ) |>
      ungroup()
  }


# group scale -------------------------------------------------------------

  if (!is.null(group)) {

    stopifnot("The group column need to be factor variable" =
                type[[rlang::as_label(qgroup)]] == "factor")

    gdata_frame <- data_frame |>
      group_by(!!qgroup, !!qtime)

  }


# assign the qtile --------------------------------------------------------

  # rank scaling
  if (scaling == "rank") {
    book <- gdata_frame |>
      # ranking the variable of interest
      mutate(
        rank = as.integer(rank(!!qvalues)),
        rank = ifelse(is.na(!!qvalues), NA, rank),
        .keep = "unused"
      ) |>
      ungroup() |>
      # split the rank into equal size bins
      mutate(
        qtile = cut(rank,
                    stats::quantile(rank,
                                    probs = seq(0, 1, 1/ngroup),
                                    na.rm = TRUE),
                    include.lowest = TRUE,
                    labels = rev(seq(1, ngroup, 1))),
        qtile = ifelse(is.na(qtile), 0, as.integer(levels(qtile)[qtile])),
        .keep = "unused"
      )
  }

  # absolute scaling
  if (scaling == "absolute") {

    # default setting for breaks
    if (is.null(breaks)) {

      breaks <- pretty(!!qvalues, ngroup)

    }

    # if the breaks vector is provided
    else {

      stopifnot("The breaks argument only accepted vector" =
                  is.vector(breaks),
                "The breaks vector must have a length greater than two" =
                  length(breaks) > 2,
                "The breaks vector should not contains NA" =
                  !is.na(breaks),
                "The breaks values is not in the range of values" =
                  all(dplyr::between(breaks,
                                     min(data[, as_label(!!qvalues)]),
                                     max(data[, as_label(!!qvalues)])))
                )

      breaks <- breaks

    }

    book <- data_frame |>
      mutate(
        qtile = cut(!!qvalues,
                    breaks,
                    include.lowest = TRUE,
                    labels = rev(seq(1, ngroup, 1))),
        qtile = ifelse(is.na(qtile), 0, as.integer(levels(qtile)[qtile])),
        .keep = "unused"
      )
  }


# rect data ---------------------------------------------------------------





}



























