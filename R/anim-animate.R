#' @import gganimate

anim_animate <- function(plot, rendering = "gganimate", ...) {
  params <- list(...)

  rendering_choice <- c("plotly", "gganimate")

  if (rendering %in% rendering_choice) {

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

    # if (rendering == "plotly") {
    #   return(
    #       plotly::ggplotly(plot)
    #   )
    # }
  }

  else {
    rlang::abort("rendering argument can only be either plotly or gganimate")
  }


}
