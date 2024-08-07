% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_methods.r
\name{is_coord3d}
\alias{is_coord3d}
\title{Test whether an object has a Coord3D class}
\usage{
is_coord3d(x)
}
\arguments{
\item{x}{An object}
}
\value{
A logical value
}
\description{
\code{is_coord3d()} tests whether an object has a "Coord3D" class
}
\examples{
p <- as_coord3d(x = sample.int(10, 3),
                y = sample.int(10, 3),
                z = sample.int(10, 3))
is_coord3d(p)
}
