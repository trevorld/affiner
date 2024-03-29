% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coord-methods.r
\name{convex_hull2d}
\alias{convex_hull2d}
\alias{convex_hull2d.Coord2D}
\title{Compute 2D convex hulls}
\usage{
convex_hull2d(x, ...)

\method{convex_hull2d}{Coord2D}(x, ...)
}
\arguments{
\item{x}{An object representing object to compute convex hull of such as a \link{Coord2D} class object.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
An object of same class as \code{x} representing just the subset of points on the convex hull.
The method for \link{Coord2D} class objects returns these points in counter-clockwise order.
}
\description{
\code{convex_hull2d()} is a S3 generic for computing the convex hull of an object.
There is an implemented method supporting \link{Coord2D} class objects
using \code{\link[grDevices:chull]{grDevices::chull()}} to compute the convex hull.
}
\examples{
p <- as_coord2d(x = rnorm(25), y = rnorm(25))
print(convex_hull2d(p))

# Equivalent to following caculation using `grDevices::chull()`
all.equal(convex_hull2d(p),
          p[rev(grDevices::chull(as.list(p)))])
}
