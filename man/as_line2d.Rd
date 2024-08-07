% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/line_plane.r
\name{as_line2d}
\alias{as_line2d}
\alias{as_line2d.numeric}
\alias{as_line2d.angle}
\alias{as_line2d.character}
\alias{as_line2d.Coord2D}
\alias{as_line2d.Line2D}
\alias{as_line2d.Point1D}
\title{Cast to Line2D object}
\usage{
as_line2d(...)

\method{as_line2d}{numeric}(a, b, c, ...)

\method{as_line2d}{angle}(theta, p1 = as_coord2d("origin"), ...)

\method{as_line2d}{character}(x, ...)

\method{as_line2d}{Coord2D}(normal, p1 = as_coord3d("origin"), p2, ...)

\method{as_line2d}{Line2D}(line, ...)

\method{as_line2d}{Point1D}(point, b = 0, ...)
}
\arguments{
\item{...}{Passed to other function such as \code{as_coord2d()}.}

\item{a, b, c}{Numeric vectors that parameterize the line via the equation \code{a * x + b * y + c = 0}.
Note if \code{y = m * x + b} then \code{m * x + 1 * y + -b = 0}.}

\item{theta}{Angle of the line represented by an \code{\link[=angle]{angle()}} vector.}

\item{p1}{Point on the line represented by a \link{Coord2D} class object.}

\item{x}{A (character) vector to be cast to a \link{Line2D} object}

\item{normal}{Normal vector to the line represented by a \link{Coord2D} class object. \code{p2} should be missing.}

\item{p2}{Another point on the line represented by a \link{Coord2D} class object.}

\item{line}{A \link{Line2D} object}

\item{point}{A \link{Point1D} object}
}
\description{
\code{as_line2d()} casts to a \link{Line2D} object.
}
\examples{
p1 <- as_coord2d(x = 5, y = 10)
p2 <- as_coord2d(x = 7, y = 12)
theta <- degrees(45)
as_line2d(theta, p1)
as_line2d(p1, p2)
}
