#'@import gganimate
#'@export

anim_animate <- function(plot, modify = FALSE) {

  check <- names(p$labels[4:5]) %in% c("ids", "frame")

  plotly <- all(check == TRUE)


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
          transition_time(frame)
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
