#' Cast to coord2d object
#'
#' `as_coord2d()` casts to a [Coord2D] class object
#'
#' @param x An object that can be cast to a [Coord2D] class object
#'          such as a matrix or data frame of coordinates.
#' @param ... Further arguments passed to or from other methods
#' @return A [Coord2D] class object
#' @examples
#' df <- data.frame(x = sample.int(10, 3),
#'                  y = sample.int(10, 3))
#' as_coord2d(df)
#' as_coord2d(complex(real = 3, imaginary = 2))
#' as_coord2d(angle(90, "degrees"), radius = 2)
#' as_coord2d(as_coord3d(1, 2, 2), alpha = degrees(90), scale = 0.5)
#' @export
as_coord2d <- function(x, ...) {
    UseMethod("as_coord2d")
}

#' Cast to coord3d object
#'
#' `as_coord3d()` casts to a [Coord3D] class object
#'
#' @param x An object that can be cast to a [Coord3D] class object
#'          such as a matrix or data frame of coordinates.
#' @param ... Further arguments passed to or from other methods
#' @return A [Coord3D] class object
#' @examples
#' as_coord3d(x = 1, y = 2, z = 3)
#' df <- data.frame(x = sample.int(10, 3),
#'                  y = sample.int(10, 3),
#'                  z = sample.int(10, 3))
#' as_coord3d(df)
#' # Cylindrical coordinates
#' as_coord3d(degrees(90), z = 1, radius = 1)
#' # Spherical coordinates
#' as_coord3d(degrees(90), inclination = degrees(90), radius = 1)
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
    as_coord2d(radius * cos(x), radius * sin(x))
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
        as_coord3d(x = radius * sin(inclination) * cos(x),
                   y = radius * sin(inclination) * sin(x),
                   z = radius * cos(inclination))
    }
}

#' @rdname as_coord2d
#' @export
as_coord2d.character <- function(x, ...) {
    xc <- vapply(x, as_coord2d_character_x, double(1), USE.NAMES = FALSE)
    yc <- vapply(x, as_coord2d_character_y, double(1), USE.NAMES = FALSE)
    p <- as_coord2d(xc, yc)
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
    p <- as_coord3d(xc, yc, zc)
    if (any(is.na(p) & !is.na(x)))
        warning("NAs introduced by coercion")
    p
}

as_coord3d_character_x <- function(x) {
    switch(x,
           "origin" = 0,
           "x-axis" = 1,
           "y-axis" = 0,
           "z-axis" = 0,
           NA_real_)
}

as_coord3d_character_y <- function(x) {
    switch(x,
           "origin" = 0,
           "x-axis" = 0,
           "y-axis" = 1,
           "z-axis" = 0,
           NA_real_)
}

as_coord3d_character_z <- function(x) {
    switch(x,
           "origin" = 0,
           "x-axis" = 0,
           "y-axis" = 0,
           "z-axis" = 1,
           NA_real_)
}

#' @rdname as_coord2d
#' @export
as_coord2d.complex <- function(x, ...) {
    as_coord2d(Re(x), Im(x))
}

#' @rdname as_coord2d
#' @param permutation Either "xyz" (no permutation), "xzy" (permute y and z axes),
#'                    "yxz" (permute x and y axes), "yzx" (x becomes z, y becomes x, z becomes y),
#'                    "zxy" (x becomes y, y becomes z, z becomes x), "zyx" (permute x and z axes).
#'                    This permutation is applied before the (oblique) projection.
#' @param plane A [Plane3D] class object representing the plane
#'         you wish to project to or an object coercible to one using `as_plane3d(plane, ...)`
#'         such as "xy-plane", "xz-plane", or "yz-plane".
#' @param scale Oblique projection foreshortening scale factor.
#'   A (degenerate) `0` value indicates an orthographic projection.
#'   A value of `0.5` is used by a \dQuote{cabinet projection}
#'   while a value of `1.0` is used by a \dQuote{cavalier projection}.
#' @param alpha Oblique projection angle (the angle the third axis is projected going off at).
#'              An [angle()] object or one coercible to one with `as_angle(alpha, ...)`.
#'              Popular angles are 45 degrees, 60 degrees, and `arctangent(2)` degrees.
#' @export
as_coord2d.Coord3D <- function(x,
                               permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy"),
                               ...,
                               plane = as_plane3d("xy-plane"),
                               scale = 0,
                               alpha = angle(45, "degrees")) {
    if (!is_plane3d(plane))
        plane <- as_plane3d(plane, ...)
    if (!is_angle(alpha)) {
        alpha <- as_angle(alpha, ...)
    }
    stopifnot(length(plane) == 1,
              plane$d == 0,
              length(alpha) == 1)
    stopifnot(length(alpha) == 1)
    permutation <- match.arg(permutation)

    azimuth <- as_angle(plane, type = "azimuth")
    inclination <- as_angle(plane, type = "inclination")

    z_axis <- Coord3D$new(matrix(c(0, 0, 1, 1), nrow = 1,
                                 dimnames = list(NULL, c("x", "y", "z", "w"))))
    y_axis <- Coord3D$new(matrix(c(0, 1, 0, 1), nrow = 1,
                                 dimnames = list(NULL, c("x", "y", "z", "w"))))
    p <- x$
    clone()$
    permute(permutation)$
    rotate(z_axis, -azimuth)$
    rotate(y_axis, -inclination)$
    shear(xz_shear = scale * cos(alpha),
          yz_shear = scale * sin(alpha))
    as_coord2d(p$x, p$y)
}

#' @rdname as_coord2d
#' @export
as_coord2d.data.frame <- function(x, ...) {
    stopifnot(all(hasName(x, c("x", "y"))))
    Coord2D$new(as_xyw_matrix(x[, c("x", "y")], ...))
}

#' @rdname as_coord3d
#' @export
as_coord3d.data.frame <- function(x, ..., z = NULL) {
    stopifnot(all(hasName(x, c("x", "y"))),
              is.null(z) || !hasName(x, "z"))
    if (!is.null(z))
        x$z <- z
    if (hasName(x, "z"))
        nms <- c("x", "y", "z")
    else
        nms <- c("x", "y")
    Coord3D$new(as_xyzw_matrix(x[, nms], ...))
}

#' @rdname as_coord2d
#' @export
as_coord2d.list <- function(x, ...) {
    as_coord2d.data.frame(as.data.frame(x, ...))
}

#' @rdname as_coord3d
#' @export
as_coord3d.list <- function(x, ..., z = NULL) {
    if (is.null(z))
        as_coord3d.data.frame(as.data.frame(x, ...))
    else
        as_coord3d.data.frame(as.data.frame(x, ...), z = z)
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
#' @param y Numeric vector of y-coordinates to be used
#'          if `hasName(x, "z")` is `FALSE`.
#' @export
as_coord2d.numeric <- function(x, y = rep_len(0, length(x)), ...) {
    xyw <- cbind(x, y, rep_len(1, max(length(x), length(y))))
    Coord2D$new(as_xyw_matrix(xyw))
}

as_xyw_matrix <- function(x) {
    if (!is.matrix(x))
        x <- as.matrix(x)
    stopifnot(ncol(x) == 2 || ncol(x) == 3,
              is.numeric(x)
    )
    if (ncol(x) < 3)
        x <- cbind(x, 1)
    else
        stopifnot(all(x[, 3] == 1))
    colnames(x) <- c("x", "y", "w")
    x
}

#' @rdname as_coord3d
#' @param y Numeric vector of y-coordinates to be used
#'          if `hasName(x, "z")` is `FALSE`.
#' @export
as_coord3d.numeric <- function(x, y = rep_len(0, length(x)), z = rep_len(0, length(x)), ...) {
    xyzw <- cbind(x, y, z, rep_len(1, max(length(x), length(y), length(z))))
    Coord3D$new(as_xyzw_matrix(xyzw))
}

as_xyzw_matrix <- function(x) {
    if (!is.matrix(x))
        x <- as.matrix(x)
    stopifnot(ncol(x) >= 2,
              ncol(x) <= 4,
              is.numeric(x)
    )
    if (ncol(x) == 2) {
        x <- cbind(x, 0, 1)
    } else if (ncol(x) == 3) {
        x <- cbind(x, 1)
    } else {
        stopifnot(all(x[, 4] == 1))
    }
    colnames(x) <- c("x", "y", "z", "w")
    x
}

#' @rdname as_coord2d
#' @export
as_coord2d.Coord2D <- function(x, ...) {
    Coord2D$new(x$xyw)
}

#' @rdname as_coord3d
#' @export
as_coord3d.Coord3D <- function(x, ...) {
    Coord3D$new(x$xyzw)
}

#' @rdname as_coord3d
#' @param z Numeric vector of z-coordinates to be used
#'          if `hasName(x, "z")` is `FALSE`.
#' @export
as_coord3d.Coord2D <- function(x, z = rep_len(0, length(x)), ...) {
    as_coord3d(x = x$x, y = x$y, z = z)
}
