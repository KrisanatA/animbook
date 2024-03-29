% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/curve-shape.R
\name{sine}
\alias{sine}
\title{Animate sine path}
\usage{
sine(x_start = 0, x_end = 1, y_start = 0, y_end = 1, n = 100)
}
\arguments{
\item{x_start}{The x-coordinate of the starting point.}

\item{x_end}{The x-coordinate of the ending point.}

\item{y_start}{The y-coordinate of the starting point.}

\item{y_end}{The y-coordinate of the ending point.}

\item{n}{A numeric value that controls the number of points between
starting and ending points.}
}
\value{
Return a path in a data frame format
}
\description{
This function creates a path from the starting point (x_start, y_start)
to the ending point (x_end, y_end) with the sine shape.
}
\examples{
path <- sine()

ggplot2::ggplot(data = path, ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()

}
\references{
Brunson JC (2020). “ggalluvial: Layered Grammar for Alluvial Plots.”
Journal of Open Source Software, 5(49), 2017. doi:10.21105/joss.02017.
}
\keyword{internal}
