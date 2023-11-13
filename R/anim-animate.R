#' Modified the ggplot object
#'
#' This function, by default will modify the ggplot object before the user can
#' pass it to the rendering of choice.
#'
#' @param plot ggplot object
#' @param modify Default is FALSE, which supplied the function needed for the rendering
#' package.
#'
#' @return A gganimate object if the rendering is gganimate or a ggplot object if
#' the rendering is plotly.
#'
#' @examples
#' animbook <- anim_prep(data = osiris, id = ID, values = sales, time = year, color = japan)
#'
#' plot <- wallaby_plot(animbook)
#'
#' animate <- anim_animate(plot)
#'
#' plotly::ggplotly(animate)
#'
#' @import gganimate plotly
#' @export

anim_animate <- function(plot, modify = FALSE) {

  stopifnot("This function only accepted an animated object." =
              "animated" %in% class(plot))

  plotly <- all(c("ids", "frame") %in% names(plot$labels))

  if (plotly == TRUE) {

    if (modify == FALSE) {

      message("You can now pass it to plotly::ggplotly()")

      return(
        plot |>
          plotly::animation_opts(1000)
      )

    }

    if (modify == TRUE) {

      message("Please supply plotly function before pass it to plotly::ggplotly()")

      return(
        plot
      )
    }


  }

  if (plotly == FALSE) {

    if (modify == FALSE) {

      message("You can now pass it to gganimate::animate()")

      return(
        plot +
          gganimate::transition_time(frame)
      )
    }

    if (modify == TRUE) {

      message("Please supply gganimate function before pass it to gganimate::animate()")

      return(
        plot
      )
    }

  }

}
