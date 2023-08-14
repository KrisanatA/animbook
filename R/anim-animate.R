#'Modified the ggplot object
#'
#'This function by default will modified the ggplot object before the user can
#'pass it to the rendering of choice.
#'
#'@param plot ggplot object
#'@param modify Default is FALSE which supplied the function needed for the rendering
#'package.
#'
#'@return A gganimate object if the rendering is gganimate or a ggplot object if
#'the rendering is plotly.
#'
#'@examples
#'data <- prep_anim(data = osiris, id = firmID, values = sales, time = year, ngroup = 5)
#'
#'label = c("Top 20%", "21-40", "41-60", "61-80", "81-100", "Not listed", "test")
#'
#'p <- anim_plot(data = data, id = firmID, time = year, color = japan, label = label)
#'
#'p2 <- anim_animate(p)
#'
#'@import gganimate plotly
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
