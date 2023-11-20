#' Modified the ggplot object
#'
#' This function will modify the ggplot object before the user can
#' pass it to the rendering of choice.
#'
#' @param plot ggplot object
#'
#' @return A gganimate object if the rendering is gganimate or a ggplot object if
#' the rendering is plotly.
#'
#' @examples
#' animbook <- anim_prep(data = osiris, id = ID, values = sales, time = year, group = japan)
#'
#' plot <- wallaby_plot(animbook)
#'
#' animate <- anim_animate(plot)
#'
#' plotly::ggplotly(animate)
#'
#' @import gganimate plotly
#' @export

anim_animate <- function(plot) {

  stopifnot("This function only accepted an animated object." =
              "animated" %in% class(plot))

  if (any("gganimate" %in% class(plot)) & any("kangaroo" %in% class(plot))) {

    width <- plot$plot_env$width

    x <- unique(plot$plot_env$data$time)

    nframes <- (width * (length(x) - 1)) + 40

    message(paste0("You can now pass it to gganimate::animate(). The recommend
            settings for nframes is ", nframes - 1, "."))

    return(plot +
             gganimate::transition_time(frame))
  }

  if (any("gganimate" %in% class(plot)) & any("wallaby" %in% class(plot))) {

    width <- plot$plot_env$width

    nframes <- width + 40

    message(paste0("You can now pass it to gganimate::animate(). The recommend
            settings for nframes is ", nframes - 1, "."))

    return(plot +
             gganimate::transition_time(frame))
  }

  if (any("plotly" %in% class(plot))) {
    message("You can now pass it to plotly::ggplotly()")

    return(
      plot |>
        plotly::animation_opts(1000)
    )
  }

}
