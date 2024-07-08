#' 1D affine transformation matrices
#'
#' `transform1d()`, `reflect1d()`, `scale2d()`,
#'  and `translate1d()` create 1D affine transformation matrix objects.
#'
#' \describe{
#' \item{`transform1d()`}{User supplied (post-multiplied) affine transformation matrix}.
#' \item{`reflect1d()`}{Reflections across a point.}
#' \item{`scale1d()`}{Scale the x-coordinates by multiplicative scale factors.}
#' \item{`translate1d()`}{Translate the coordinates by a [Coord1D] class object parameter.}
#' }
#'
#' `transform1d()` 1D affine transformation matrix objects are meant to be
#' post-multiplied and therefore should **not** be multiplied in reverse order.
#' Note the [Coord1D] class object methods auto-pre-multiply affine transformations
#' when "method chaining" so pre-multiplying affine transformation matrices
#' to do a single cumulative transformation instead of a method chain of multiple transformations
#' will not improve performance as much as as it does in other R packages.
#'
#' To convert a pre-multiplied 1D affine transformation matrix to a post-multiplied one
#' simply compute its transpose using [t()].  To get an inverse transformation matrix
#' from an existing transformation matrix that does the opposite transformations
#' simply compute its inverse using [solve()].
#'
#' @return A 2x2 post-multiplied affine transformation matrix with classes "transform1d" and "at_matrix"
#'
#' @param mat A 2x2 matrix representing a post-multiplied affine transformation matrix.
#'            The last **column** must be equal to `c(0, 1)`.
#'            If the last **row** is `c(0, 1)` you may need to transpose it
#'            to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.
#'            If a 1x1 matrix we'll quietly add a final column/row equal to `c(0, 1)`.
#' @examples
#' p <- as_coord1d(x = sample(1:10, 3))
#'
#' # {affiner} affine transformation matrices are post-multiplied
#' # and therefore should **not** go in reverse order
#' mat <- transform1d(diag(2)) %*%
#'          scale1d(2) %*%
#'          translate1d(x = -1)
#' p1 <- p$
#'   clone()$
#'   transform(mat)
#'
#' # The equivalent result appyling affine transformations via method chaining
#' p2 <- p$
#'   clone()$
#'   transform(diag(2))$
#'   scale(2)$
#'   translate(x = -1)
#'
#' all.equal(p1, p2)
#' @export
transform1d <- function(mat = diag(2L)) {
    if (all(dim(mat) == c(1, 1))) {
        mat <- rbind(mat, 0)
        mat <- cbind(mat, c(0, 1))
    }
    validate_transform1d(mat)
    new_transform1d(mat)
}

#' 2D affine transformation matrices
#'
#' `transform2d()`, `project2d()`, `reflect2d()`, `rotate2d()`, `scale2d()`, `shear2d()`,
#'  and `translate2d()` create 2D affine transformation matrix objects.
#'
#' \describe{
#' \item{`transform2d()`}{User supplied (post-multiplied) affine transformation matrix}.
#' \item{`project2d()`}{Oblique vector projections onto a line parameterized by
#'                      an oblique projection scale factor.
#'                      A (degenerate) scale factor of zero results in an orthogonal projection.}
#' \item{`reflect2d()`}{Reflections across a line.
#'                      To "flip" across both the x-axis and the y-axis use `scale2d(-1)`.}
#' \item{`rotate2d()`}{Rotations around the origin parameterized by an [angle()].}
#' \item{`scale2d()`}{Scale the x-coordinates and/or the y-coordinates by multiplicative scale factors.}
#' \item{`shear2d()`}{Shear the x-coordinates and/or the y-coordinates using shear factors.}
#' \item{`translate2d()`}{Translate the coordinates by a [Coord2D] class object parameter.}
#' }
#'
#' `transform2d()` 2D affine transformation matrix objects are meant to be
#' post-multiplied and therefore should **not** be multiplied in reverse order.
#' Note the [Coord2D] class object methods auto-pre-multiply affine transformations
#' when "method chaining" so pre-multiplying affine transformation matrices
#' to do a single cumulative transformation instead of a method chain of multiple transformations
#' will not improve performance as much as as it does in other R packages.
#'
#' To convert a pre-multiplied 2D affine transformation matrix to a post-multiplied one
#' simply compute its transpose using [t()].  To get an inverse transformation matrix
#' from an existing transformation matrix that does the opposite transformations
#' simply compute its inverse using [solve()].
#'
#' @return A 3x3 post-multiplied affine transformation matrix with classes "transform2d" and "at_matrix"
#'
#' @param mat A 3x3 matrix representing a post-multiplied affine transformation matrix.
#'            The last **column** must be equal to `c(0, 0, 1)`.
#'            If the last **row** is `c(0, 0, 1)` you may need to transpose it
#'            to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.
#'            If a 2x2 matrix (such as a 2x2 post-multiplied 2D rotation matrix)
#'            we'll quietly add a final column/row equal to `c(0, 0, 1)`.
#' @examples
#' p <- as_coord2d(x = sample(1:10, 3), y = sample(1:10, 3))
#'
#' # {affiner} affine transformation matrices are post-multiplied
#' # and therefore should **not** go in reverse order
#' mat <- transform2d(diag(3)) %*%
#'          reflect2d(as_coord2d(-1, 1)) %*%
#'          rotate2d(90, "degrees") %*%
#'          scale2d(1, 2) %*%
#'          shear2d(0.5, 0.5) %*%
#'          translate2d(x = -1, y = -1)
#' p1 <- p$
#'   clone()$
#'   transform(mat)
#'
#' # The equivalent result appyling affine transformations via method chaining
#' p2 <- p$
#'   clone()$
#'   transform(diag(3L))$
#'   reflect(as_coord2d(-1, 1))$
#'   rotate(90, "degrees")$
#'   scale(1, 2)$
#'   shear(0.5, 0.5)$
#'   translate(x = -1, y = -1)
#'
#' all.equal(p1, p2)
#'
#' @export
transform2d <- function(mat = diag(3L)) {
    if (all(dim(mat) == c(2, 2))) {
        mat <- rbind(mat, c(0, 0))
        mat <- cbind(mat, c(0, 0, 1))
    }
    validate_transform2d(mat)
    new_transform2d(mat)
}

#' 3D affine transformation matrices
#'
#' `transform3d()`, `project3d()`, `reflect3d()`, `rotate3d()`, `scale3d()`, `shear3d()`,
#'  and `translate3d()` create 3D affine transformation matrix objects.
#'
#' \describe{
#' \item{`transform3d()`}{User supplied (post-multiplied) affine transformation matrix}.
#' \item{`scale3d()`}{Scale the x-coordinates and/or the y-coordinates and/or the z-coordinates
#'                    by multiplicative scale factors.}
#' \item{`shear3d()`}{Shear the x-coordinates and/or the y-coordinates
#'                    and/or the z-coordinates using shear factors.}
#' \item{`translate3d()`}{Translate the coordinates by a [Coord3D] class object parameter.}
#' }
#'
#' `transform3d()` 3D affine transformation matrix objects are meant to be
#' post-multiplied and therefore should **not** be multiplied in reverse order.
#' Note the [Coord3D] class object methods auto-pre-multiply affine transformations
#' when "method chaining" so pre-multiplying affine transformation matrices
#' to do a single cumulative transformation instead of a method chain of multiple transformations
#' will not improve performance as much as as it does in other R packages.
#'
#' To convert a pre-multiplied 3D affine transformation matrix to a post-multiplied one
#' simply compute its transpose using [t()].  To get an inverse transformation matrix
#' from an existing transformation matrix that does the opposite transformations
#' simply compute its inverse using [solve()].
#'
#' @return A 4x4 post-multiplied affine transformation matrix with classes "transform3d" and "at_matrix"
#'
#' @param mat A 4x4 matrix representing a post-multiplied affine transformation matrix.
#'            The last **column** must be equal to `c(0, 0, 0, 1)`.
#'            If the last **row** is `c(0, 0, 0, 1)` you may need to transpose it
#'            to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.
#'            If a 3x3 matrix (such as a 3x3 post-multiplied 3D rotation matrix)
#'            we'll quietly add a final column/row equal to `c(0, 0, 0, 1)`.
#' @examples
#' p <- as_coord3d(x = sample(1:10, 3), y = sample(1:10, 3), z = sample(1:10, 3))
#'
#' # {affiner} affine transformation matrices are post-multiplied
#' # and therefore should **not** go in reverse order
#' mat <- transform3d(diag(4L)) %*%
#'          rotate3d("z-axis", degrees(90)) %*%
#'          scale3d(1, 2, 1) %*%
#'          translate3d(x = -1, y = -1, z = -1)
#' p1 <- p$
#'   clone()$
#'   transform(mat)
#'
#' # The equivalent result appyling affine transformations via method chaining
#' p2 <- p$
#'   clone()$
#'   transform(diag(4L))$
#'   rotate("z-axis", degrees(90))$
#'   scale(1, 2, 1)$
#'   translate(x = -1, y = -1, z = -1)
#'
#' all.equal(p1, p2)
#'
#' @export
transform3d <- function(mat = diag(4L)) {
    if (all(dim(mat) == c(3, 3))) {
        mat <- rbind(mat, c(0, 0, 0))
        mat <- cbind(mat, c(0, 0, 0, 1))
    }
    validate_transform3d(mat)
    new_transform3d(mat)
}

validate_transform1d <- function(x) {
    stopifnot(is.matrix(x),
              all(dim(x) == c(2L, 2L)))
    if (!all(x[, 2L] == c(0, 1))) {
        msg <- "The last column must be equal to `c(0, 1)`."
        if (all(x[2L, ] == c(0, 1))) {
            stop(paste(msg,
                       "Do you need to transpose a pre-multiplied affine transformation matrix",
                       "with `t()` to convert it into a post-multiplied one?"))
        } else {
            stop(msg)
        }
    }
}

validate_transform2d <- function(x) {
    stopifnot(is.matrix(x),
              all(dim(x) == c(3, 3)))
    if (!all(x[, 3L] == c(0, 0, 1))) {
        msg <- "The last column must be equal to `c(0, 0, 1)`."
        if (all(x[3L, ] == c(0, 0, 1))) {
            stop(paste(msg,
                       "Do you need to transpose a pre-multiplied affine transformation matrix",
                       "with `t()` to convert it into a post-multiplied one?"))
        } else {
            stop(msg)
        }
    }
}

validate_transform3d <- function(x) {
    stopifnot(is.matrix(x),
              all(dim(x) == c(4, 4)))
    if (!all(x[, 4L] == c(0, 0, 0, 1))) {
        msg <- "The last column must be equal to `c(0, 0, 0, 1)`."
        if (all(x[4L, ] == c(0, 0, 0, 1))) {
            stop(paste(msg,
                       "Do you need to transpose a pre-multiplied affine transformation matrix",
                       "with `t()` to convert it into a post-multiplied one?"))
        } else {
            stop(msg)
        }
    }
}

new_transform1d <- function(mat) {
    if (!inherits(mat, "transform1d"))
        class(mat) <- c("transform1d", "at_matrix", class(matrix()))
    mat
}

new_transform2d <- function(mat) {
    if (!inherits(mat, "transform2d"))
        class(mat) <- c("transform2d", "at_matrix", class(matrix()))
    mat
}

new_transform3d <- function(mat) {
    if (!inherits(mat, "transform3d"))
        class(mat) <- c("transform3d", "at_matrix", class(matrix()))
    mat
}

#' @rdname transform2d
#' @param permutation Either "xy" (no permutation) or "yx" (permute x and y axes)
#' @export
permute2d <- function(permutation = c("xy", "yx")) {
    permutation <- match.arg(permutation)
    mat <- switch(permutation,
           xy = diag(3L),
           yx = matrix(c(0, 1, 0,
                         1, 0, 0,
                         0, 0, 1), byrow = TRUE, ncol = 3, nrow = 3))
    new_transform2d(mat)
}

#' @rdname transform3d
#' @param permutation Either "xyz" (no permutation), "xzy" (permute y and z axes),
#'                    "yxz" (permute x and y axes), "yzx" (x becomes z, y becomes x, z becomes y),
#'                    "zxy" (x becomes y, y becomes z, z becomes x), "zyx" (permute x and z axes)
#' @export
permute3d <- function(permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy")) {
    permutation <- match.arg(permutation)
    mat <- switch(permutation,
           xyz = diag(4L),
           xzy = matrix(c(1, 0, 0, 0,
                          0, 0, 1, 0,
                          0, 1, 0, 0,
                          0, 0, 0, 1), byrow = TRUE, ncol = 4, nrow = 4),
           yxz = matrix(c(0, 1, 0, 0,
                          1, 0, 0, 0,
                          0, 0, 1, 0,
                          0, 0, 0, 1), byrow = TRUE, ncol = 4, nrow = 4),
           yzx = matrix(c(0, 0, 1, 0,
                          1, 0, 0, 0,
                          0, 1, 0, 0,
                          0, 0, 0, 1), byrow = TRUE, ncol = 4, nrow = 4),
           zxy = matrix(c(0, 1, 0, 0,
                          0, 0, 1, 0,
                          1, 0, 0, 0,
                          0, 0, 0, 1), byrow = TRUE, ncol = 4, nrow = 4),
           zyx = matrix(c(0, 0, 1, 0,
                          0, 1, 0, 0,
                          1, 0, 0, 0,
                          0, 0, 0, 1), byrow = TRUE, ncol = 4, nrow = 4))
    new_transform2d(mat)
}

#' @rdname transform1d
#' @export
project1d <- function(point = as_point1d("origin"), ...) {
    if (!is_point1d(point))
        point <- as_point1d(point, ...)
    stopifnot(length(point) == 1L)
    x <- as_coord1d(point)
    mat <- scale1d(0) %*% translate1d(x)
    new_transform1d(mat)
}

#' @rdname transform2d
#' @param scale Oblique projection scale factor.
#'   A degenerate `0` value indicates an orthogonal projection.
#' @export
project2d <- function(line = as_line2d("x-axis"), ..., scale = 0) {
    if (!is_line2d(line))
        line <- as_line2d(line, ...)
    stopifnot(length(line) == 1L,
              scale == 0 || line$a == 0)
    denom <- line$a^2 + line$b^2
    closest <- as_coord2d(-line$a * line$c / denom, -line$b * line$c / denom)
    theta <- as_angle(line)
    mat <- translate2d(-closest) %*%
        rotate2d(-theta) %*%
        shear2d(xy_shear = scale) %*%
        scale2d(y_scale = 0) %*%
        rotate2d(theta) %*%
        translate2d(closest)
    new_transform2d(mat)
}

#' @rdname transform3d
#' @param scale Oblique projection foreshortening scale factor.
#'   A (degenerate) `0` value indicates an orthographic projection.
#'   A value of `0.5` is used by a \dQuote{cabinet projection}
#'   while a value of `1.0` is used by a \dQuote{cavalier projection}.
#' @param alpha Oblique projection angle (the angle the third axis is projected going off at).
#'              An [angle()] object or one coercible to one with `as_angle(alpha, ...)`.
#'              Popular angles are 45 degrees, 60 degrees, and `arctangent(2)` degrees.
#' @export
project3d <- function(plane = as_plane3d("xy-plane"), ...,
                      scale = 0,
                      alpha = angle(45, "degrees")) {
    if (!is_plane3d(plane))
        plane <- as_plane3d(plane, ...)
    if (!is_angle(alpha)) {
        alpha <- as_angle(alpha, ...)
    }
    stopifnot(length(plane) == 1L,
              scale == 0 || (plane$a == 0 && plane$b == 0),
              length(alpha) == 1L)
    denom <- plane$a^2 + plane$b^2 + plane$c^2
    closest <- as_coord3d(-plane$a * plane$d / denom, -plane$b * plane$d / denom, -plane$c * plane$d / denom)
    azimuth <- as_angle(plane, type = "azimuth")
    inclination <- as_angle(plane, type = "inclination")
    z_axis <- Coord3D$new(matrix(c(0, 0, 1, 1), nrow = 1,
                                 dimnames = list(NULL, c("x", "y", "z", "w"))))
    y_axis <- Coord3D$new(matrix(c(0, 1, 0, 1), nrow = 1,
                                 dimnames = list(NULL, c("x", "y", "z", "w"))))
    mat <- translate3d(-closest) %*%
        rotate3d(z_axis, -azimuth) %*%
        rotate3d(y_axis, -inclination) %*%
        shear3d(xz_shear = scale * cos(alpha),
                yz_shear = scale * sin(alpha)) %*%
        scale3d(z_scale = 0) %*%
        rotate3d(y_axis, inclination) %*%
        rotate3d(z_axis, azimuth) %*%
        translate3d(closest)
    new_transform3d(mat)
}

#' @rdname transform1d
#' @param point A [Point1D] object of length one or an object coercible to one by `as_point1d(point, ...)`.
#'             For `project1d()` and `reflect1d()` it represents the
#'             the point you wish to project to or reflect across.
#' @export
reflect1d <- function(point = as_point1d("origin"), ...) {
    if (!is_point1d(point))
        point <- as_point1d(point, ...)
    stopifnot(length(point) == 1)
    x <- as_coord1d(point)
    mat <- translate1d(-x) %*%
        scale1d(-1) %*%
        translate1d(x)
    new_transform1d(mat)
}

#' @rdname transform2d
#' @param line A [Line2D] object of length one or an object coercible to one by `as_line2d(line, ...)`.
#'             For `project2d()` and `reflect2d()` it represents the
#'             the line you wish to project to or reflect across.
#' @export
reflect2d <- function(line = as_line2d("x-axis"), ...) {
    if (!is_line2d(line))
        line <- as_line2d(line, ...)
    stopifnot(length(line) == 1L)
    denom <- line$a^2 + line$b^2
    closest <- as_coord2d(-line$a * line$c / denom, -line$b * line$c / denom)
    theta <- as_angle(line)
    mat <- translate2d(-closest) %*%
        rotate2d(-theta) %*%
        scale2d(1, -1) %*%
        rotate2d(theta) %*%
        translate2d(closest)
    new_transform2d(mat)
}

# https://en.wikipedia.org/wiki/Transformation_matrix#Reflection_2

#' @rdname transform3d
#' @param plane A [Plane3D] class object representing the plane
#'         you wish to reflect across or project to or an object coercible to one using `as_plane3d(plane, ...)`
#'         such as "xy-plane", "xz-plane", or "yz-plane".
#' @export
reflect3d <- function(plane = as_plane3d("xy-plane"), ...) {
    if (!is_plane3d(plane))
        plane <- as_plane3d(plane, ...)
    stopifnot(length(plane) == 1)
    normal <- normal3d(plane)
    denom <- plane$a^2 + plane$b^2 + plane$c^2
    closest <- as_coord3d(-plane$a * plane$d / denom, -plane$b * plane$d / denom, -plane$c * plane$d / denom)

    mat <- diag(4L)
    mat[1L, 1L] <- 1 - 2 * normal$x^2
    mat[1L, 2L] <- mat[2L, 1L] <- -2 * normal$x * normal$y
    mat[1L, 3L] <- mat[3L, 1L] <- -2 * normal$x * normal$z
    mat[2L, 2L] <- 1 - 2 * normal$y^2
    mat[2L, 3L] <- mat[3L, 2L] <- -2 * normal$y * normal$z
    mat[3L, 3L] <- 1 - 2 * normal$z^2

    mat <- translate3d(-closest) %*% mat %*% translate3d(closest)
    new_transform3d(mat)
}

#' @rdname transform2d
#' @param theta An [angle()] object of length one or an object coercible to one by `as_angle(theta, ...)`.
#' @param ... Passed to [as_angle()] or [as_coord2d()].
#' @export
rotate2d <- function(theta = angle(0), ...) {
    if (!is_angle(theta))
        theta <- as_angle(theta, ...)
    stopifnot(length(theta) == 1)
    mat <- matrix(c(cos(theta), sin(theta), 0,
                   -sin(theta), cos(theta), 0,
                    0,          0,          1),
                  nrow = 3L, ncol = 3L, byrow = TRUE)
    new_transform2d(mat)
}

# Axis-angle representation to rotation matrix
# https://en.wikipedia.org/wiki/Axis-angle_representation
# Because we do rotation matrix post-multiplication instead of pre-multiplication we usually need to multiply angles
# in following algorithms by -1

#' @param axis A [Coord3D] class object or one that can coerced to one by `as_coord3d(axis, ...)`.
#'             The `axis` represents the axis to be rotated around.
#' @param theta An [angle()] object of length one or an object coercible to one by `as_angle(theta, ...)`.
#' @rdname transform3d
#' @export
rotate3d <- function(axis = as_coord3d("z-axis"), theta = angle(0), ...) {
    if (!is_coord3d(axis))
        axis <- as_coord3d(axis, ...)
    stopifnot(length(axis) == 1)
    axis <- axis / abs(axis)

    if (!is_angle(theta))
        theta <- as_angle(theta, ...)
    stopifnot(length(theta) == 1)

    I <- diag(3L)
    K <- cross_matrix(axis)
    c <- cos(-theta)
    s <- sin(-theta)
    R <- I + s * K + (1 - c) * K %*% K

    mat <- diag(4L)
    mat[1:3, 1:3] <- R
    mat
}

# "cross" product matrix
# https://en.wikipedia.org/wiki/Cross_product#Conversion_to_matrix_multiplication
cross_matrix <- function(x) {
    stopifnot(is_coord3d(x) && length(x) == 1L)
    m <- matrix(0, nrow = 3L, ncol = 3L)
    m[1, 2] <- -x$z
    m[1, 3] <- x$y
    m[2, 1] <- x$z
    m[2, 3] <- -x$x
    m[3, 1] <- -x$y
    m[3, 2] <- x$x
    m
}

#' @rdname transform1d
#' @param x_scale Scaling factor to apply to x coordinates
#' @export
scale1d <- function(x_scale = 1) {
    stopifnot(length(x_scale) == 1L)
    mat <- diag(2L)
    mat[1L, 1L] <- x_scale
    new_transform1d(mat)
}

#' @rdname transform2d
#' @param x_scale Scaling factor to apply to x coordinates
#' @param y_scale Scaling factor to apply to y coordinates
#' @export
scale2d <- function(x_scale = 1, y_scale = x_scale) {
    stopifnot(length(x_scale) == 1L && length(y_scale) == 1L)
    mat <- diag(3L)
    mat[1L, 1L] <- x_scale
    mat[2L, 2L] <- y_scale
    new_transform2d(mat)
}

#' @rdname transform3d
#' @param x_scale Scaling factor to apply to x coordinates
#' @param y_scale Scaling factor to apply to y coordinates
#' @param z_scale Scaling factor to apply to z coordinates
#' @export
scale3d <- function(x_scale = 1, y_scale = x_scale, z_scale = x_scale) {
    stopifnot(length(x_scale) == 1L && length(y_scale) == 1L && length(z_scale) == 1L)
    mat <- diag(4L)
    mat[1L, 1L] <- x_scale
    mat[2L, 2L] <- y_scale
    mat[3L, 3L] <- z_scale
    new_transform3d(mat)
}

#' @rdname transform2d
#' @param xy_shear Horizontal shear factor: `x = x + xy_shear * y`
#' @param yx_shear Vertical shear factor: `y = yx_shear * x + y`
#' @export
shear2d <- function(xy_shear = 0, yx_shear = 0) {
    stopifnot(length(xy_shear) == 1L && length(yx_shear) == 1L)
    mat <- diag(3L)
    mat[2L, 1L] <- xy_shear
    mat[1L, 2L] <- yx_shear
    new_transform2d(mat)
}

#' @rdname transform3d
#' @param xy_shear Shear factor: `x = x + xy_shear * y + xz_shear * z`
#' @param xz_shear Shear factor: `x = x + xy_shear * y + xz_shear * z`
#' @param yx_shear Shear factor: `y = yx_shear * x + y + yz_shear * z`
#' @param yz_shear Shear factor: `y = yx_shear * x + y + yz_shear * z`
#' @param zx_shear Shear factor: `z = zx_shear * x + zy_shear * y + z`
#' @param zy_shear Shear factor: `z = zx_shear * x + zy_shear * y + z`
#' @export
shear3d <- function(xy_shear = 0, xz_shear = 0,
                    yx_shear = 0, yz_shear = 0,
                    zx_shear = 0, zy_shear = 0) {
    stopifnot(all(lengths(list(xy_shear, xz_shear, yx_shear, yz_shear, zx_shear, zy_shear)) == 1))
    mat <- diag(4L)
    mat[2L, 1L] <- xy_shear
    mat[3L, 1L] <- xz_shear
    mat[1L, 2L] <- yx_shear
    mat[3L, 2L] <- yz_shear
    mat[1L, 3L] <- zx_shear
    mat[2L, 3L] <- zy_shear
    new_transform2d(mat)
}

#' @rdname transform1d
#' @param x A [Coord1D] object of length one or an object coercible to one by `as_coord1d(x, ...)`].
#' @param ... Passed to [as_coord1d()].
#' @export
translate1d <- function(x = as_coord1d(0), ...) {
    if (!is_coord1d(x))
        x <- as_coord1d(x, ...)
    stopifnot(length(x) == 1L)
    mat <- diag(2L)
    mat[2L, 1L] <- x$x
    new_transform1d(mat)
}

#' @rdname transform2d
#' @param x A [Coord2D] object of length one or an object coercible to one by `as_coord2d(x, ...)`].
#' @export
translate2d <- function(x = as_coord2d(0, 0), ...) {
    if (!is_coord2d(x))
        x <- as_coord2d(x, ...)
    stopifnot(length(x) == 1L)
    mat <- diag(3L)
    mat[3L, 1L] <- x$x
    mat[3L, 2L] <- x$y
    new_transform2d(mat)
}

#' @rdname transform3d
#' @param x A [Coord3D] object of length one or an object coercible to one by `as_coord3d(x, ...)`].
#' @param ... Passed to [as_angle()] or [as_coord3d()].
#' @export
translate3d <- function(x = as_coord3d(0, 0, 0), ...) {
    if (!is_coord3d(x))
        x <- as_coord3d(x, ...)
    stopifnot(length(x) == 1L)
    mat <- diag(4L)
    mat[4L, 1L] <- x$x
    mat[4L, 2L] <- x$y
    mat[4L, 3L] <- x$z
    new_transform3d(mat)
}
