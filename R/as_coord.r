#' Cast to coord2d object
#'
#' `as_coord2d()` casts to a [coord2d()] object
#'
#' @param x An object that can be cast to a [coord2d()] object
#'          such as a matrix or data frame of coordinates.
#' @param ... Further arguments passed to or from other methods
#' @return A [coord2d()] object
#' @examples
#' df <- data.frame(x = sample.int(10, 3),
#'                  y = sample.int(10, 3))
#' as_coord2d(df)
#' @export
as_coord2d <- function(x, ...) {
    UseMethod("as_coord2d")
}

#' Cast to coord3d object
#'
#' `as_coord3d()` casts to a [coord3d()] object
#'
#' @param x An object that can be cast to a [coord3d()] object
#'          such as a matrix or data frame of coordinates.
#' @param ... Further arguments passed to or from other methods
#' @return A [coord3d()] object
#' @examples
#' df <- data.frame(x = sample.int(10, 3),
#'                  y = sample.int(10, 3),
#'                  z = sample.int(10, 3))
#' as_coord3d(df)
#' @export
as_coord3d <- function(x, ...) {
    UseMethod("as_coord3d")
}

#' @rdname as_coord2d
#' @param radius A numeric vector of radial distances.
#' @export
as_coord2d.angle <- function(x, radius = 1, ...) {
    n <- max(length(x), length(radius))
    x <- rep_len(x, n)
    radius <- rep_len(radius, n)
    coord2d(radius * cos(x), radius * sin(x))
}

#' @rdname as_coord3d
#' @param radius A numeric vector.  If `inclination` is not `NULL` represents spherical distances
#'               of spherical coordinates and if `z` is not `NULL` represents
#'               radial distances of cylindrical coordinates.
#' @param inclination Spherical coordinates inclination angle aka polar angle.
#'                    `x` represents the azimuth aka azimuthal angle.
#' @export
as_coord3d.angle <- function(x, radius = 1, inclination = NULL, z = NULL, ...) {
    stopifnot(!(is.null(inclination) && is.null(z)),
              is.null(inclination) || is.null(z))
    if (!is.null(inclination) && !is_angle(inclination))
        inclination <- as_angle(inclination, ...)
    if (!is.null(z) && !is.numeric(z))
        z <- as.numeric(z)
    if (is.null(inclination)) { # cylindrical coordinates
        as_coord3d(as_coord2d(x, radius = radius), z = z)
    } else { # spherical coordinates
        coord3d(x = radius * sin(inclination) * cos(x),
                y = radius * sin(inclination) * sin(x),
                z = radius * cos(inclination))
    }
}

#' @rdname as_coord2d
#' @export
as_coord2d.character <- function(x, ...) {
    xc <- vapply(x, as_coord2d_character_x, double(1), USE.NAMES = FALSE)
    yc <- vapply(x, as_coord2d_character_y, double(1), USE.NAMES = FALSE)
    p <- coord2d(xc, yc)
    if (any(is.na(p) & !is.na(x)))
        warning("NAs introduced by coercion")
    p
}

as_coord2d_character_x <- function(x) {
    switch(x,
           "origin" = 0,
           "x-axis" = 1,
           "y-axis" = 0,
           NA_real_)
}

as_coord2d_character_y <- function(x) {
    switch(x,
           "origin" = 0,
           "x-axis" = 0,
           "y-axis" = 1,
           NA_real_)
}

#' @rdname as_coord3d
#' @export
as_coord3d.character <- function(x, ...) {
    xc <- vapply(x, as_coord3d_character_x, double(1), USE.NAMES = FALSE)
    yc <- vapply(x, as_coord3d_character_y, double(1), USE.NAMES = FALSE)
    zc <- vapply(x, as_coord3d_character_z, double(1), USE.NAMES = FALSE)
    p <- coord3d(xc, yc, zc)
    if (any(is.na(p) & !is.na(x)))
        warning("NAs introduced by coercion")
    p
}

as_coord3d_character_x <- function(x) {
    switch(x,
           "origin" = 0,
           "x-axis" = 1,
           "yz-plane" = 1,
           "zy-plane" = -1,
           "y-axis" = 0,
           "xz-plane" = 0,
           "zx-plane" = 0,
           "z-axis" = 0,
           "xy-plane" = 0,
           "yx-plane" = 0,
           NA_real_)
}

as_coord3d_character_y <- function(x) {
    switch(x,
           "origin" = 0,
           "x-axis" = 0,
           "yz-plane" = 0,
           "zy-plane" = 0,
           "y-axis" = 1,
           "xz-plane" = -1,
           "zx-plane" = 1,
           "z-axis" = 0,
           "xy-plane" = 0,
           "yx-plane" = 0,
           NA_real_)
}

as_coord3d_character_z <- function(x) {
    switch(x,
           "origin" = 0,
           "x-axis" = 0,
           "yz-plane" = 0,
           "zy-plane" = 0,
           "y-axis" = 0,
           "xz-plane" = 0,
           "zx-plane" = 0,
           "z-axis" = 1,
           "xy-plane" = 1,
           "yx-plane" = -1,
           NA_real_)
}

#' @rdname as_coord2d
#' @export
as_coord2d.complex <- function(x, ...) {
    coord2d(Re(x), Im(x))
}

#' @rdname as_coord2d
#' @param permutation Either "xyz" (no permutation), "xzy" (permute y and z axes),
#'                    "yxz" (permute x and y axes), "yzx" (x becomes z, y becomes x, z becomes y),
#'                    "zxy" (x becomes y, y becomes z, z becomes x), "zyx" (permute x and z axes).
#'                    This permutation is applied before the (oblique) projection.
#' @param normal A [coord3d()] object representing the vector normal of the plane
#'         you wish to project to or an object coercible to one using `as_coord3d(normal, ...)`
#'         such as "xy-plane", "xz-plane", or "yz-plane".
#'         We will also (if necessary) coerce it to a unit vector.
#' @param scale Oblique projection foreshortening scale factor.
#'   A (degenerate) `0` value indicates an orthographic projection.
#'   A value of `0.5` is used by a \dQuote{cabinet projection}
#'   while a value of `1.0` is used by a \dQuote{cavalier projection}.
#' @param alpha Oblique projection angle (the angle the third axis is projected going off at).
#'              An [angle()] object or one coercible to one with `as_angle(alpha, ...)`.
#'              Popular angles are 45 degrees, 60 degrees, and `arctangent(2)` degrees.
#' @export
as_coord2d.coord3d <- function(x,
                               permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy"),
                               ...,
                               normal = as_coord3d("xy-plane"),
                               scale = 0,
                               alpha = angle(45, "degrees")) {
    if (!is_coord3d(normal))
        normal <- as_coord3d(normal, ...)
    stopifnot(length(normal) == 1)
    if (!is_angle(alpha)) {
        alpha <- as_angle(alpha, ...)
    }
    stopifnot(length(alpha) == 1)
    permutation <- match.arg(permutation)

    normal <- normal / abs(normal)
    azimuth <- as_angle(normal, type = "azimuth")
    inclination <- as_angle(normal, type = "inclination")

    z_axis <- coord3d(0, 0, 1)
    y_axis <- coord3d(0, 1, 0)
    p <- x$
    clone()$
    permute(permutation)$
    rotate(z_axis, -azimuth)$
    rotate(y_axis, -inclination)$
    shear(xz_shear = scale * cos(alpha),
          yz_shear = scale * sin(alpha))
    coord2d(p$x, p$y)
}

#' @rdname as_coord2d
#' @export
as_coord2d.data.frame <- function(x, ...) {
    if (all(hasName(x, c("x", "y"))))
        x <- x[, c("x", "y")]
    Coord2D$new(as_xyw_matrix(x, ...))
}

#' @rdname as_coord3d
#' @export
as_coord3d.data.frame <- function(x, ...) {
    if (all(hasName(x, c("x", "y", "z"))))
        x <- x[, c("x", "y", "z")]
    Coord3D$new(as_xyzw_matrix(x, ...))
}

#' @rdname as_coord2d
#' @export
as_coord2d.list <- function(x, ...) {
    as_coord2d.data.frame(as.data.frame(x, ...))
}

#' @rdname as_coord3d
#' @export
as_coord3d.list <- function(x, ...) {
    as_coord3d.data.frame(as.data.frame(x, ...))
}

#' @rdname as_coord2d
#' @export
as_coord2d.matrix <- function(x, ...) {
    Coord2D$new(as_xyw_matrix(x))
}

#' @rdname as_coord3d
#' @export
as_coord3d.matrix <- function(x, ...) {
    Coord3D$new(as_xyzw_matrix(x))
}

#' @rdname as_coord2d
#' @export
as_coord2d.coord2d <- function(x, ...) {
    Coord2D$new(x$xyw)
}

#' @rdname as_coord3d
#' @export
as_coord3d.coord3d <- function(x, ...) {
    Coord3D$new(x$xyzw)
}

#' @rdname as_coord3d
#' @param z Numeric vector of z-coordinates
#' @export
as_coord3d.coord2d <- function(x, z = rep_len(0, length(x)), ...) {
    coord3d(x = x$x, y = x$y, z = z)
}
