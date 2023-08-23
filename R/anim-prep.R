anim_prep <- function(data,
                      id = NULL,
                      values = NULL,
                      time = NULL,
                      label = NULL,
                      ngroup = 5,
                      breaks = NULL,
                      time_dependent = TRUE,
                      scaling = "rank",
                      runif_min = 1,
                      runif_max = 50) {

# enquo -------------------------------------------------------------------

  qid <- rlang::enquo(id)
  qvalues <- rlang::enquo(values)
  qtime <- rlang::enquo(time)

  type <- sapply(data, class)


# check column class ------------------------------------------------------

  stopifnot("The id column need to be factor variable" =
              type[[rlang::as_label(id)]] == "factor",
            "The values column need to be numeric variable" =
              type[[rlang::as_label(values)]] == "numeric",
            "The time column need to be integer variable" =
              type[[rlang::as_label(time)]] == "integer")


# scaling choice ----------------------------------------------------------

scaling_choic <- c("rank", "absolute")


# assign the frames -------------------------------------------------------

  if (time_dependent == FALSE) {
    data_frame <- data |>
      arrange(!!qid, !!qtime) |>
      group_by(!!qid) |>
      mutate(
        frame = dplyr::row_number(),
        frame = frame + floor(runif(1, 1, 50))
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




# assign the qtile --------------------------------------------------------

  if (scaling == "rank") {
    book <- data_frame |>
      group_by(!!qtime) |>
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

  if (scaling == "absolute") {

    if (is.null(breaks)) {

      breaks <- pretty(!!qvalues, ngroup)

    }

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

}



























