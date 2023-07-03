#' Compute Euclidean norm
#'
#' `abs()` computes the Euclidean norm for [Coord2D] class objects and [Coord3D] class objects.
#' @param x A [Coord2D] class object or [Coord2D] class object.
#' @examples
#'   z <- complex(real = 1:4, imaginary = 1:4)
#'   p <- as_coord2d(z)
#'   abs(p) # Euclidean norm
#'   # Less efficient ways to calculate same Euclidean norms
#'   sqrt(p * p) # `*` dot product
#'   distance2d(p, as_coord2d(0, 0, 0))
#'
#'   # In {base} R `abs()` calculates Euclidean norm of complex numbers
#'   all.equal(abs(p), abs(z))
#'   all.equal(Mod(p), Mod(z))
#'
#'   p3 <- as_coord3d(x = 1:4, y = 1:4, z = 1:4)
#'   abs(p3)
#' @return A numeric vector
#' @export
abs.Coord2D <- function(x) {
    sqrt(rowSums(x$xyw[, 1:2, drop = FALSE]^2))
}

#' @rdname abs.Coord2D
#' @export
abs.Coord3D <- function(x) {
    sqrt(rowSums(x$xyzw[, 1:3, drop = FALSE]^2))
}

#' 2D Euclidean distances
#'
#' `distance2d()` computes 2D Euclidean distances.
#'
#' @param x Either a [Coord2D] or [Line2D] class object
#' @param y Either a [Coord2D] or [Line2D] class object
#' @examples
#'   p <- as_coord2d(x = 1:4, y = 1:4)
#'   distance2d(p, as_coord2d(0, 0))
#' @export
distance2d <- function(x, y) {
    if (is_coord2d(x)) {
        distance2d_coord2d(x, y)
    } else if (is_line2d(x)) {
        distance2d_line2d(x, y)
    } else {
        stop(paste("Don't know how to handle", dQuote(class(x)), "class object"))
    }
}

distance2d_coord2d <- function(x, y) {
    if (is_coord2d(y)) {
        distance2d_coord2d_coord2d(x, y)
    } else if (is_line2d(y)) {
        distance2d_coord2d_line2d(x, y)
    } else {
        stop(paste("Don't know how to handle", dQuote(class(y)), "class object"))
    }
}

distance2d_line2d <- function(x, y) {
    if (is_coord2d(y)) {
        distance2d_coord2d_line2d(y, x)
    } else {
        stop(paste("Don't know how to handle", dQuote(class(y)), "class object"))
    }
}

distance2d_coord2d_coord2d <- function(x, y) {
    n <- max(length(x), length(y))
    x <- rep_len(x, n)
    y <- rep_len(y, n)
    sqrt(rowSums((x$xyw[, 1:2, drop = FALSE] - y$xyw[, 1:2, drop = FALSE])^2))
}

distance2d_coord2d_line2d <- function(x, y) {
    abs(y$a * x$x + y$b * x$y + y$c) / sqrt(y$a^2 + y$b^2)
}

#' 3D Euclidean distances
#'
#' `distance3d()` computes 3D Euclidean distances.
#'
#' @param x Either a [Coord3D] or [Plane3D] class object
#' @param y Either a [Coord3D] or [Plane3D] class object
#' @examples
#'   p <- as_coord3d(x = 1:4, y = 1:4, z = 1:4)
#'   distance3d(p, as_coord3d("origin"))
#' @export
distance3d <- function(x, y) {
    if (is_coord3d(x)) {
        distance3d_coord3d(x, y)
    } else if (is_plane3d(x)) {
        distance3d_plane3d(x, y)
    } else {
        stop(paste("Don't know how to handle", dQuote(class(x)), "class object"))
    }
}

distance3d_coord3d <- function(x, y) {
    if (is_coord3d(y)) {
        distance3d_coord3d_coord3d(x, y)
    } else if (is_plane3d(y)) {
        distance3d_coord3d_plane3d(x, y)
    } else {
        stop(paste("Don't know how to handle", dQuote(class(y)), "class object"))
    }
}

distance3d_plane3d <- function(x, y) {
    if (is_coord3d(y)) {
        distance3d_coord3d_plane3d(y, x)
    } else {
        stop(paste("Don't know how to handle", dQuote(class(y)), "class object"))
    }
}

distance3d_coord3d_coord3d <- function(x, y) {
    n <- max(length(x), length(y))
    x <- rep_len(x, n)
    y <- rep_len(y, n)
    sqrt(rowSums((x$xyzw[, 1:3, drop = FALSE] - y$xyzw[, 1:3, drop = FALSE])^2))
}

distance3d_coord3d_plane3d <- function(x, y) {
    n <- max(length(x), length(y))
    x <- rep_len(x, n)
    y <- rep_len(y, n)
    abs(y$a * x$x + y$b * x$y + y$c * x$z + y$d) / sqrt(y$a^2 + y$b^2 + y$c^2)
}
