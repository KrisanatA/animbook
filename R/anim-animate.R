#'@import gganimate
#'@export

anim_animate <- function(plot, rendering = "gganimate", ...) {
  params <- list(...)

  rendering_choice <- c("plotly", "gganimate")

  stopifnot("rendering argument can only be either gganimate or plotly" = rendering %in% rendering_choice)

  if (rendering == "gganimate") {

    if (length(params) == 0) {
      return(
        plot +
          transition_time(frame)
      )
    }

    if (length(params) > 0) {
      return(
        plot +
          params)
    }
  }

  if (rendering == "plotly") {


  }

}
