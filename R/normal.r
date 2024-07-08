#' 2D normal vectors
#'
#' `normal2d()` is an S3 generic that computes a 2D normal vector.
#'
#' @examples
#'   p <- as_coord2d(x = 2, y = 3)
#'   normal2d(p)
#'   normal2d(p, normalize = FALSE)
#' @return A [Coord2D] (normal) vector
#' @rdname normal2d
#' @param x Object to compute a 2D normal vector for
#'          such as a [Line2D] object.
#' @param normalize If `TRUE` coerce to a normalize vector
#' @param ... Passed to or from other methods.
#' @export
normal2d <- function(x, ...) {
    UseMethod("normal2d")
}

#' @rdname normal2d
#' @export
normal2d.Coord2D <- function(x, ..., normalize = TRUE) {
    # Two possible normals:
    # 1: (x, y) => (y, -x) / abs((x, y))
    # 2: (x, y) => (-y, x) / abs((x, y))
    # we arbitrarily went with option 1
    n <- x$clone()$
            scale(-1, 1)$
            permute("yx")
    if (normalize)
        n$scale(1 / abs(x))
    n
}

#' 3D normal vectors
#'
#' `normal3d()` is an S3 generic that computes a 3D normal vector.
#'
#' @examples
#' normal3d("xy-plane")
#' normal3d(as_coord3d(2, 0, 0), cross = as_coord3d(0, 2, 0))
#' @return A [Coord3D] (normal) vector
#' @export
normal3d <- function(x, ...) {
    UseMethod("normal3d")
}

#' @param cross A [Coord3D] vector.
#'              We'll compute the normal of `x` and `cross` by taking their cross product.
#' @param normalize If `TRUE` normalize to a unit vector
#' @param ... Passed to other methods such as [as_coord3d()].
#' @rdname normal3d
#' @export
normal3d.Coord3D <- function(x, cross, ..., normalize = TRUE) {
    if (!is_coord3d(cross))
        cross <- as_coord3d(cross, ...)
    n <- cross_product3d(x, cross)
    if (normalize)
        n$scale(1 / abs(n))
    n
}

#' @param x Object to compute a 3D normal vector for
#'          such as a [Plane3D] object
#' @rdname normal3d
#' @export
normal3d.character <- function(x, ..., normalize = TRUE) {
    xc <- vapply(x, normal3d_character_x, double(1), USE.NAMES = FALSE)
    yc <- vapply(x, normal3d_character_y, double(1), USE.NAMES = FALSE)
    zc <- vapply(x, normal3d_character_z, double(1), USE.NAMES = FALSE)
    p <- as_coord3d(xc, yc, zc)
    if (any(is.na(p) & !is.na(x)))
        warning("NAs introduced by coercion")
    p$scale(1 / abs(p))
    p
}

normal3d_character_x <- function(x) {
    switch(x,
           "yz-plane" = 1,
           "zy-plane" = -1,
           "xz-plane" = 0,
           "zx-plane" = 0,
           "xy-plane" = 0,
           "yx-plane" = 0,
           NA_real_)
}

normal3d_character_y <- function(x) {
    switch(x,
           "yz-plane" = 0,
           "zy-plane" = 0,
           "xz-plane" = -1,
           "zx-plane" = 1,
           "xy-plane" = 0,
           "yx-plane" = 0,
           NA_real_)
}

normal3d_character_z <- function(x) {
    switch(x,
           "yz-plane" = 0,
           "zy-plane" = 0,
           "xz-plane" = 0,
           "zx-plane" = 0,
           "xy-plane" = 1,
           "yx-plane" = -1,
           NA_real_)
}

#' @rdname normal2d
#' @export
normal2d.Line2D <- function(x, ..., normalize = TRUE) {
    n <- as_coord2d(x$a, x$b)
    n$scale(1 / abs(n))
    n
}

#' @rdname normal3d
#' @export
normal3d.Plane3D <- function(x, ..., normalize = TRUE) {
    n <- as_coord3d(x$a, x$b, x$c)
    n$scale(1 / abs(n))
    n
}
