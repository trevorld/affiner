% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_equivalent.r
\name{is_equivalent}
\alias{is_equivalent}
\alias{is_equivalent.angle}
\alias{is_equivalent.numeric}
\alias{is_equivalent.Coord1D}
\alias{is_equivalent.Coord2D}
\alias{is_equivalent.Coord3D}
\alias{is_equivalent.Point1D}
\alias{is_equivalent.Line2D}
\alias{is_equivalent.Plane3D}
\title{Test whether two objects are equivalent}
\usage{
is_equivalent(x, y, ...)

\method{is_equivalent}{angle}(
  x,
  y,
  ...,
  mod_turns = TRUE,
  tolerance = sqrt(.Machine$double.eps)
)

\method{is_equivalent}{numeric}(x, y, ..., tolerance = sqrt(.Machine$double.eps))

\method{is_equivalent}{Coord1D}(x, y, ..., tolerance = sqrt(.Machine$double.eps))

\method{is_equivalent}{Coord2D}(x, y, ..., tolerance = sqrt(.Machine$double.eps))

\method{is_equivalent}{Coord3D}(x, y, ..., tolerance = sqrt(.Machine$double.eps))

\method{is_equivalent}{Point1D}(x, y, ..., tolerance = sqrt(.Machine$double.eps))

\method{is_equivalent}{Line2D}(x, y, ..., tolerance = sqrt(.Machine$double.eps))

\method{is_equivalent}{Plane3D}(x, y, ..., tolerance = sqrt(.Machine$double.eps))
}
\arguments{
\item{x, y}{Two objects to test whether they are \dQuote{"equivalent"}.}

\item{...}{Further arguments passed to or from other methods.}

\item{mod_turns}{If \code{TRUE} angles that are congruent modulo full turns will be considered \dQuote{congruent}.}

\item{tolerance}{Numerics with differences smaller
than \code{tolerance} will be considered \dQuote{equivalent}.}
}
\value{
A logical vector
}
\description{
\code{is_equivalent()} is a S3 generic that tests whether two different objects are \dQuote{equivalent}.
The \code{is_equivalent()} method for \code{\link[=angle]{angle()}} classes tests whether two angles are congruent.
The \code{is_equivalent()} method for \link{Point1D}, \link{Line2D}, \link{Plane3D} classes tests whether they are the same point/line/plane after standardization.
}
\examples{
line1 <- as_line2d(a = 1, b = 2, c = 3) # 1 * x + 2 * y + 3 = 0
line2 <- as_line2d(a = 2, b = 4, c = 6) # 2 * x + 4 * y + 6 = 0
is_equivalent(line1, line2)
}
\seealso{
\code{\link[=is_congruent]{is_congruent()}}, \code{\link[=all.equal]{all.equal()}}
}
