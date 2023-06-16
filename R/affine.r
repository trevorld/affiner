#' 2D affine transformation matrices
#'
#' `transform2d()`, `project2d()`, `reflect2d()`, `rotate2d()`, `scale2d()`, `shear2d()`,
#'  and `translate2d()` create 2D affine transformation matrix objects.
#'
#' \describe{
#' \item{`transform2d()`}{User supplied (post-multiplied) affine transformation matrix}.
#' \item{`project2d()`}{Oblique vector projections onto a unit vector parameterized by its (polar) [angle()] from the x-axis
#'                      and an oblique projection scale factor.
#'                      A (degenerate) scale factor of zero results in an orthogonal projection.}
#' \item{`reflect2d()`}{Reflections across a line parameterized by its [angle()] from the x-axis.
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
#'   p <- as_coord2d(x = sample(1:10, 3), y = sample(1:10, 3))
#'
#'   # {affiner} affine transformation matrices are post-multiplied
#'   # and therefore should **not** go in reverse order
#'   mat <- transform2d(diag(3)) %*%
#'            reflect2d(as_coord2d(-1, 1)) %*%
#'            rotate2d(90, "degrees") %*%
#'            scale2d(1, 2) %*%
#'            shear2d(0.5, 0.5) %*%
#'            translate2d(x = -1, y = -1)
#'   p1 <- p$
#'     clone()$
#'     transform(mat)
#'
#'   # The equivalent result appyling affine transformations via method chaining
#'   p2 <- p$
#'     clone()$
#'     transform(diag(3))$
#'     reflect(as_coord2d(-1, 1))$
#'     rotate(90, "degrees")$
#'     scale(1, 2)$
#'     shear(0.5, 0.5)$
#'     translate(x = -1, y = -1)
#'
#'   all.equal(p1, p2)
#'
#' @export
transform2d <- function(mat = diag(3)) {
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
#'   p <- as_coord3d(x = sample(1:10, 3), y = sample(1:10, 3), z = sample(1:10, 3))
#'
#'   # {affiner} affine transformation matrices are post-multiplied
#'   # and therefore should **not** go in reverse order
#'   mat <- transform3d(diag(4)) %*%
#'            rotate3d("z-axis", degrees(90)) %*%
#'            scale3d(1, 2, 1) %*%
#'            translate3d(x = -1, y = -1, z = -1)
#'   p1 <- p$
#'     clone()$
#'     transform(mat)
#'
#'   # The equivalent result appyling affine transformations via method chaining
#'   p2 <- p$
#'     clone()$
#'     transform(diag(4))$
#'     rotate("z-axis", degrees(90))$
#'     scale(1, 2, 1)$
#'     translate(x = -1, y = -1, z = -1)
#'
#'   all.equal(p1, p2)
#'
#' @export
transform3d <- function(mat = diag(4)) {
    if (all(dim(mat) == c(3, 3))) {
        mat <- rbind(mat, c(0, 0, 0))
        mat <- cbind(mat, c(0, 0, 0, 1))
    }
    validate_transform3d(mat)
    new_transform3d(mat)
}

validate_transform2d <- function(x) {
    stopifnot(is.matrix(x),
              all(dim(x) == c(3, 3)))
    if (!all(x[, 3] == c(0, 0, 1))) {
        msg <- "The last column must be equal to `c(0, 0, 1)`."
        if (all(x[3, ] == c(0, 0, 1))) {
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
    if (!all(x[, 4] == c(0, 0, 0, 1))) {
        msg <- "The last column must be equal to `c(0, 0, 0, 1)`."
        if (all(x[4, ] == c(0, 0, 0, 1))) {
            stop(paste(msg,
                       "Do you need to transpose a pre-multiplied affine transformation matrix",
                       "with `t()` to convert it into a post-multiplied one?"))
        } else {
            stop(msg)
        }
    }
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
           xy = diag(3),
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
           xyz = diag(4),
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

#' @rdname transform2d
#' @param scale Oblique projection scale factor.
#'   A degenerate `0` value indicates an orthogonal projection.
#' @export
project2d <- function(theta = angle(0), ..., scale = 0) {
    if (!is_angle(theta))
        theta <- as_angle(theta, ...)
    stopifnot(length(theta) == 1)
    mat <- rotate2d(-theta) %*%
        shear2d(xy_shear = scale) %*%
        scale2d(y_scale = 0) %*%
        rotate2d(theta)
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
project3d <- function(normal = as_coord3d("xy-plane"), ...,
                      scale = 0,
                      alpha = angle(45, "degrees")) {
    if (!is_coord3d(normal))
        normal <- as_coord3d(normal, ...)
    stopifnot(length(normal) == 1)
    if (!is_angle(alpha)) {
        alpha <- as_angle(alpha, ...)
    }
    stopifnot(length(alpha) == 1)
    normal <- normal / abs(normal)
    azimuth <- as_angle(normal, type = "azimuth")
    inclination <- as_angle(normal, type = "inclination")
    z_axis <- Coord3D$new(matrix(c(0, 0, 1, 1), nrow = 1,
                                 dimnames = list(NULL, c("x", "y", "z", "w"))))
    y_axis <- Coord3D$new(matrix(c(0, 1, 0, 1), nrow = 1,
                                 dimnames = list(NULL, c("x", "y", "z", "w"))))
    mat <- rotate3d(z_axis, -azimuth) %*%
        rotate3d(y_axis, -inclination) %*%
        shear3d(xz_shear = scale * cos(alpha),
                yz_shear = scale * sin(alpha)) %*%
        scale3d(z_scale = 0) %*%
        rotate3d(y_axis, inclination) %*%
        rotate3d(z_axis, azimuth)
    new_transform3d(mat)
}

#' @rdname transform2d
#' @export
reflect2d <- function(theta = as_angle("x-axis"), ...) {
    if (!is_angle(theta))
        theta <- as_angle(theta, ...)
    stopifnot(length(theta) == 1)
    mat <- rotate2d(-theta) %*%
        scale2d(1, -1) %*%
        rotate2d(theta)
    new_transform2d(mat)
}

# https://en.wikipedia.org/wiki/Transformation_matrix#Reflection_2

#' @rdname transform3d
#' @param normal A [Coord3D] class object representing the vector normal of the plane
#'         you wish to reflect across or project to or an object coercible to one using `as_coord3d(normal, ...)`
#'         such as "xy-plane", "xz-plane", or "yz-plane".
#'         We will also (if necessary) coerce it to a unit vector.
#' @export
reflect3d <- function(normal = as_coord3d("xy-plane"), ...) {
    if (!is_coord3d(normal))
        normal <- as_coord3d(normal, ...)
    stopifnot(length(normal) == 1)
    normal <- normal / abs(normal)
    mat <- diag(4)
    mat[1, 1] <- 1 - 2 * normal$x^2
    mat[1, 2] <- mat[2, 1] <- -2 * normal$x * normal$y
    mat[1, 3] <- mat[3, 1] <- -2 * normal$x * normal$z
    mat[2, 2] <- 1 - 2 * normal$y^2
    mat[2, 3] <- mat[3, 2] <- -2 * normal$y * normal$z
    mat[3, 3] <- 1 - 2 * normal$z^2
    new_transform3d(mat)
}

#' @rdname transform2d
#' @param theta An [angle()] object of length one or an object coercible to one by `as_angle(theta, ...)`.
#'              For `rotate2d()` how much to rotate around the origin.
#'              For `project2d()` and `reflect2d()` it represents the angle (from the horizontal axis)
#'              of the line going through the origin you wish to project to or reflect across
#'              (e.g. an angle of 0 corresponds to the x-axis and
#'              an angle of 90 degrees corresponds to the y-axis).
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

    I <- diag(3)
    K <- cross_matrix(axis)
    c <- cos(-theta)
    s <- sin(-theta)
    R <- I + s * K + (1 - c) * K %*% K

    mat <- diag(4)
    mat[1:3, 1:3] <- R
    mat
}

#' @rdname transform2d
#' @param x_scale Scaling factor to apply to x coordinates
#' @param y_scale Scaling factor to apply to y coordinates
#' @export
scale2d <- function(x_scale = 1, y_scale = x_scale) {
    stopifnot(length(x_scale) == 1 && length(y_scale) == 1)
    mat <- diag(3)
    mat[1, 1] <- x_scale
    mat[2, 2] <- y_scale
    new_transform2d(mat)
}

#' @rdname transform3d
#' @param x_scale Scaling factor to apply to x coordinates
#' @param y_scale Scaling factor to apply to y coordinates
#' @param z_scale Scaling factor to apply to z coordinates
#' @export
scale3d <- function(x_scale = 1, y_scale = x_scale, z_scale = x_scale) {
    stopifnot(length(x_scale) == 1 && length(y_scale) == 1 && length(z_scale) == 1)
    mat <- diag(4)
    mat[1, 1] <- x_scale
    mat[2, 2] <- y_scale
    mat[3, 3] <- z_scale
    new_transform3d(mat)
}

#' @rdname transform2d
#' @param xy_shear Horizontal shear factor: `x = x + xy_shear * y`
#' @param yx_shear Vertical shear factor: `y = yx_shear * x + y`
#' @export
shear2d <- function(xy_shear = 0, yx_shear = 0) {
    stopifnot(length(xy_shear) == 1 && length(yx_shear) == 1)
    mat <- diag(3)
    mat[2, 1] <- xy_shear
    mat[1, 2] <- yx_shear
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
    mat <- diag(4)
    mat[2, 1] <- xy_shear
    mat[3, 1] <- xz_shear
    mat[1, 2] <- yx_shear
    mat[3, 2] <- yz_shear
    mat[1, 3] <- zx_shear
    mat[2, 3] <- zy_shear
    new_transform2d(mat)
}

#' @rdname transform2d
#' @param x A [Coord2D] object of length one or an object coercible to one by `as_coord2d(x, ...)`].
#' @export
translate2d <- function(x = as_coord2d(0, 0), ...) {
    if (!is_coord2d(x))
        x <- as_coord2d(x, ...)
    stopifnot(length(x) == 1)
    mat <- diag(3)
    mat[3, 1] <- x$x
    mat[3, 2] <- x$y
    new_transform2d(mat)
}

#' @rdname transform3d
#' @param x A [Coord3D] object of length one or an object coercible to one by `as_coord3d(x, ...)`].
#' @param ... Passed to [as_angle()] or [as_coord3d()].
#' @export
translate3d <- function(x = as_coord3d(0, 0, 0), ...) {
    if (!is_coord3d(x))
        x <- as_coord3d(x, ...)
    stopifnot(length(x) == 1)
    mat <- diag(4)
    mat[4, 1] <- x$x
    mat[4, 2] <- x$y
    mat[4, 3] <- x$z
    new_transform3d(mat)
}
