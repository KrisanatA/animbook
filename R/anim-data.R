anim_data <- function(df, id, y) {
  df |>
    dplyr::group_by({{id}}) |>
    dplyr::arrange({{id}}) |>
    dplyr::mutate(time = row_number(),
           time = time + floor(runif(1, 10, 100)),
           y = {{y}} + runif(1, -0.1, 0.1))
}
