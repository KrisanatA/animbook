#' @import gganimate

anim_animate <- function(plot, ...) {
  params <- list(...)

  if (length(params) == 0) {
    return(
      plot +
        transition_time(time)
    )
  }

  if (length(params) > 0) {
    return(
      plot +
        params)
  }
}
