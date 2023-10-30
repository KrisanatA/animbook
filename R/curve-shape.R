#' Animate sigmoid path
#'
#' This function creates a path from the starting point (x_start, y_start)
#' to the ending point (x_end, y_end) with the sigmoid shape.
#'
#' @param x_start The x-coordinate of the starting point.
#' @param x_end The x-coordinate of the ending point.
#' @param y_start The y-coordinate of the starting point.
#' @param y_end The y-coordinate of the ending point.
#' @param scale A numeric value that controls the length of the sigmoid.
#' @param n A numeric value that controls the number of points between
#' starting and ending points.
#'
#' @return Return a path in a data frame format
#'
#' @keywords internal
#'
#' @examples
#' path <- sigmoid()
#'
#' ggplot2::ggplot(data = path, ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()
#'
#' @references
#' Recreate - Sankey flow chart. (n.d.). Emil Hvitfeldt.
#'  Retrieved April 13, 2023, from
#'  https://www.emilhvitfeldt.com/post/2018-03-20-recreate-sankey-flow-chart/
#'
#' @export

sigmoid <- function(x_start = 0,
                    x_end = 1,
                    y_start = 0,
                    y_end = 1,
                    scale = 5,
                    n = 100) {

  if(!is.numeric(c(x_start, x_end, y_start, y_end, scale, n))) {
    stop("All the input must be numeric.")
  }

  else {
    x <- seq(-scale, scale, length = n)
    y <- exp(x) / (exp(x) + 1)
    data.frame(x = (x + scale) / (scale * 2) * (x_end - x_start) + x_start,
           y = y * (y_end - y_start) + y_start)
  }

}


#' Animate sine path
#'
#' This function creates a path from the starting point (x_start, y_start)
#' to the ending point (x_end, y_end) with the sine shape.
#'
#' @param x_start The x-coordinate of the starting point.
#' @param x_end The x-coordinate of the ending point.
#' @param y_start The y-coordinate of the starting point.
#' @param y_end The y-coordinate of the ending point.
#' @param n A numeric value that controls the number of points between
#' starting and ending points.
#'
#' @return Return a path in a data frame format
#'
#' @keywords internal
#'
#' @examples
#' path <- sine()
#'
#' ggplot2::ggplot(data = path, ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()
#'
#' @references
#' Brunson JC (2020). “ggalluvial: Layered Grammar for Alluvial Plots.”
#'   Journal of Open Source Software, 5(49), 2017. doi:10.21105/joss.02017.
#'
#' @export

sine <- function(x_start = 0,
                 x_end = 1,
                 y_start = 0,
                 y_end = 1,
                 n = 100) {

  if(!is.numeric(c(x_start, x_end, y_start, y_end, n))) {
    stop("All the input must be numeric.")
  }

  else {
    x <- seq(x_start, x_end, length = n)
    y <- sin((x - 0.5) * pi) / 2 + 0.5
    data.frame(x = x,
               y = y * (y_end - y_start) + y_start)
  }

}


