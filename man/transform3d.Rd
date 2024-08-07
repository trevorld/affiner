% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/affine.r
\name{transform3d}
\alias{transform3d}
\alias{permute3d}
\alias{project3d}
\alias{reflect3d}
\alias{rotate3d}
\alias{scale3d}
\alias{shear3d}
\alias{translate3d}
\title{3D affine transformation matrices}
\usage{
transform3d(mat = diag(4L))

permute3d(permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy"))

project3d(
  plane = as_plane3d("xy-plane"),
  ...,
  scale = 0,
  alpha = angle(45, "degrees")
)

reflect3d(plane = as_plane3d("xy-plane"), ...)

rotate3d(axis = as_coord3d("z-axis"), theta = angle(0), ...)

scale3d(x_scale = 1, y_scale = x_scale, z_scale = x_scale)

shear3d(
  xy_shear = 0,
  xz_shear = 0,
  yx_shear = 0,
  yz_shear = 0,
  zx_shear = 0,
  zy_shear = 0
)

translate3d(x = as_coord3d(0, 0, 0), ...)
}
\arguments{
\item{mat}{A 4x4 matrix representing a post-multiplied affine transformation matrix.
The last \strong{column} must be equal to \code{c(0, 0, 0, 1)}.
If the last \strong{row} is \code{c(0, 0, 0, 1)} you may need to transpose it
to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.
If a 3x3 matrix (such as a 3x3 post-multiplied 3D rotation matrix)
we'll quietly add a final column/row equal to \code{c(0, 0, 0, 1)}.}

\item{permutation}{Either "xyz" (no permutation), "xzy" (permute y and z axes),
"yxz" (permute x and y axes), "yzx" (x becomes z, y becomes x, z becomes y),
"zxy" (x becomes y, y becomes z, z becomes x), "zyx" (permute x and z axes)}

\item{plane}{A \link{Plane3D} object of length one representing the plane
you wish to reflect across or project to or an object coercible to one using \code{as_plane3d(plane, ...)}
such as "xy-plane", "xz-plane", or "yz-plane".}

\item{...}{Passed to \code{\link[=as_angle]{as_angle()}} or \code{\link[=as_coord3d]{as_coord3d()}}.}

\item{scale}{Oblique projection foreshortening scale factor.
A (degenerate) \code{0} value indicates an orthographic projection.
A value of \code{0.5} is used by a \dQuote{cabinet projection}
while a value of \code{1.0} is used by a \dQuote{cavalier projection}.}

\item{alpha}{Oblique projection angle (the angle the third axis is projected going off at).
An \code{\link[=angle]{angle()}} object or one coercible to one with \code{as_angle(alpha, ...)}.
Popular angles are 45 degrees, 60 degrees, and \code{arctangent(2)} degrees.}

\item{axis}{A \link{Coord3D} class object or one that can coerced to one by \code{as_coord3d(axis, ...)}.
The \code{axis} represents the axis to be rotated around.}

\item{theta}{An \code{\link[=angle]{angle()}} object of length one or an object coercible to one by \code{as_angle(theta, ...)}.}

\item{x_scale}{Scaling factor to apply to x coordinates}

\item{y_scale}{Scaling factor to apply to y coordinates}

\item{z_scale}{Scaling factor to apply to z coordinates}

\item{xy_shear}{Shear factor: \code{x = x + xy_shear * y + xz_shear * z}}

\item{xz_shear}{Shear factor: \code{x = x + xy_shear * y + xz_shear * z}}

\item{yx_shear}{Shear factor: \code{y = yx_shear * x + y + yz_shear * z}}

\item{yz_shear}{Shear factor: \code{y = yx_shear * x + y + yz_shear * z}}

\item{zx_shear}{Shear factor: \code{z = zx_shear * x + zy_shear * y + z}}

\item{zy_shear}{Shear factor: \code{z = zx_shear * x + zy_shear * y + z}}

\item{x}{A \link{Coord3D} object of length one or an object coercible to one by \code{as_coord3d(x, ...)}.}
}
\value{
A 4x4 post-multiplied affine transformation matrix with classes "transform3d" and "at_matrix"
}
\description{
\code{transform3d()}, \code{project3d()}, \code{reflect3d()}, \code{rotate3d()}, \code{scale3d()}, \code{shear3d()},
and \code{translate3d()} create 3D affine transformation matrix objects.
}
\details{
\describe{
\item{\code{transform3d()}}{User supplied (post-multiplied) affine transformation matrix}.
\item{\code{scale3d()}}{Scale the x-coordinates and/or the y-coordinates and/or the z-coordinates
by multiplicative scale factors.}
\item{\code{shear3d()}}{Shear the x-coordinates and/or the y-coordinates
and/or the z-coordinates using shear factors.}
\item{\code{translate3d()}}{Translate the coordinates by a \link{Coord3D} class object parameter.}
}

\code{transform3d()} 3D affine transformation matrix objects are meant to be
post-multiplied and therefore should \strong{not} be multiplied in reverse order.
Note the \link{Coord3D} class object methods auto-pre-multiply affine transformations
when "method chaining" so pre-multiplying affine transformation matrices
to do a single cumulative transformation instead of a method chain of multiple transformations
will not improve performance as much as as it does in other R packages.

To convert a pre-multiplied 3D affine transformation matrix to a post-multiplied one
simply compute its transpose using \code{\link[=t]{t()}}.  To get an inverse transformation matrix
from an existing transformation matrix that does the opposite transformations
simply compute its inverse using \code{\link[=solve]{solve()}}.
}
\examples{
p <- as_coord3d(x = sample(1:10, 3), y = sample(1:10, 3), z = sample(1:10, 3))

# {affiner} affine transformation matrices are post-multiplied
# and therefore should **not** go in reverse order
mat <- transform3d(diag(4L)) \%*\%
         rotate3d("z-axis", degrees(90)) \%*\%
         scale3d(1, 2, 1) \%*\%
         translate3d(x = -1, y = -1, z = -1)
p1 <- p$
  clone()$
  transform(mat)

# The equivalent result appyling affine transformations via method chaining
p2 <- p$
  clone()$
  transform(diag(4L))$
  rotate("z-axis", degrees(90))$
  scale(1, 2, 1)$
  translate(x = -1, y = -1, z = -1)

all.equal(p1, p2)

}
