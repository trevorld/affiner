% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/affine.r
\name{transform1d}
\alias{transform1d}
\alias{project1d}
\alias{reflect1d}
\alias{scale1d}
\alias{translate1d}
\title{1D affine transformation matrices}
\usage{
transform1d(mat = diag(2L))

project1d(point = as_point1d("origin"), ...)

reflect1d(point = as_point1d("origin"), ...)

scale1d(x_scale = 1)

translate1d(x = as_coord1d(0), ...)
}
\arguments{
\item{mat}{A 2x2 matrix representing a post-multiplied affine transformation matrix.
The last \strong{column} must be equal to \code{c(0, 1)}.
If the last \strong{row} is \code{c(0, 1)} you may need to transpose it
to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.
If a 1x1 matrix we'll quietly add a final column/row equal to \code{c(0, 1)}.}

\item{point}{A \link{Point1D} object of length one representing the point
you with to reflect across or project to or an object coercible to one by \code{as_point1d(point, ...)}
such as "origin".}

\item{...}{Passed to \code{\link[=as_coord1d]{as_coord1d()}}.}

\item{x_scale}{Scaling factor to apply to x coordinates}

\item{x}{A \link{Coord1D} object of length one or an object coercible to one by \code{as_coord1d(x, ...)}.}
}
\value{
A 2x2 post-multiplied affine transformation matrix with classes "transform1d" and "at_matrix"
}
\description{
\code{transform1d()}, \code{reflect1d()}, \code{scale2d()},
and \code{translate1d()} create 1D affine transformation matrix objects.
}
\details{
\describe{
\item{\code{transform1d()}}{User supplied (post-multiplied) affine transformation matrix}.
\item{\code{reflect1d()}}{Reflections across a point.}
\item{\code{scale1d()}}{Scale the x-coordinates by multiplicative scale factors.}
\item{\code{translate1d()}}{Translate the coordinates by a \link{Coord1D} class object parameter.}
}

\code{transform1d()} 1D affine transformation matrix objects are meant to be
post-multiplied and therefore should \strong{not} be multiplied in reverse order.
Note the \link{Coord1D} class object methods auto-pre-multiply affine transformations
when "method chaining" so pre-multiplying affine transformation matrices
to do a single cumulative transformation instead of a method chain of multiple transformations
will not improve performance as much as as it does in other R packages.

To convert a pre-multiplied 1D affine transformation matrix to a post-multiplied one
simply compute its transpose using \code{\link[=t]{t()}}.  To get an inverse transformation matrix
from an existing transformation matrix that does the opposite transformations
simply compute its inverse using \code{\link[=solve]{solve()}}.
}
\examples{
p <- as_coord1d(x = sample(1:10, 3))

# {affiner} affine transformation matrices are post-multiplied
# and therefore should **not** go in reverse order
mat <- transform1d(diag(2)) \%*\%
         scale1d(2) \%*\%
         translate1d(x = -1)
p1 <- p$
  clone()$
  transform(mat)

# The equivalent result appyling affine transformations via method chaining
p2 <- p$
  clone()$
  transform(diag(2))$
  scale(2)$
  translate(x = -1)

all.equal(p1, p2)
}
