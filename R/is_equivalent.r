#' Test whether two objects are equivalent
#'
#' `is_equivalent()` is a S3 generic that tests whether two different objects are \dQuote{equivalent}.
#' The `is_equivalent()` method for [angle()] classes tests whether two angles are congruent.
#' The `is_equivalent()` method for [Point1D], [Line2D], [Plane3D] classes tests whether they are the same point/line/plane after standardization.
#'
#' @param x,y Two objects to test whether they are \dQuote{"equivalent"}.
#' @param ... Further arguments passed to or from other methods.
#' @return A logical vector
#'
#' @examples
#' line1 <- as_line2d(a = 1, b = 2, c = 3) # 1 * x + 2 * y + 3 = 0
#' line2 <- as_line2d(a = 2, b = 4, c = 6) # 2 * x + 4 * y + 6 = 0
#' is_equivalent(line1, line2)
#' @seealso [is_congruent()], [all.equal()]
#' @export
is_equivalent <- function(x, y, ...) {
    UseMethod("is_equivalent")
}

#' @param mod_turns If `TRUE` angles that are congruent modulo full turns will be considered \dQuote{congruent}.
#' @param tolerance Numerics with differences smaller
#'                  than `tolerance` will be considered \dQuote{equivalent}.
#' @rdname is_equivalent
#' @export
is_equivalent.angle <- function(x, y, ...,
                                mod_turns = TRUE,
                                tolerance = sqrt(.Machine$double.eps))
    is_congruent.angle(x, y, ..., mod_turns = mod_turns, tolerance = tolerance)

#' @rdname is_equivalent
#' @export
is_equivalent.numeric <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps))
    is_congruent.numeric(x, y, ..., tolerance = tolerance)

#' @rdname is_equivalent
#' @export
is_equivalent.Coord1D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
    if (!is_coord1d(y)) {
        y <- as_coord1d(y)
    }
    stopifnot(!any(is_degenerate(x)),
              !any(is_degenerate(y)))
    n <- max(length(x), length(y))
    x <- rep(x, length.out = n)
    y <- rep(y, length.out = n)
    is_equivalent(x$x, y$x, tolerance = tolerance)
}

#' @rdname is_equivalent
#' @export
is_equivalent.Coord2D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
    if (!is_coord2d(y)) {
        y <- as_coord2d(y)
    }
    stopifnot(!any(is_degenerate(x)),
              !any(is_degenerate(y)))
    n <- max(length(x), length(y))
    x <- rep(x, length.out = n)
    y <- rep(y, length.out = n)
    is_equivalent(x$x, y$x, tolerance = tolerance) &
        is_equivalent(x$y, y$y, tolerance = tolerance)
}

#' @rdname is_equivalent
#' @export
is_equivalent.Coord3D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
    if (!is_coord3d(y)) {
        y <- as_coord3d(y)
    }
    stopifnot(!any(is_degenerate(x)),
              !any(is_degenerate(y)))
    n <- max(length(x), length(y))
    x <- rep(x, length.out = n)
    y <- rep(y, length.out = n)
    is_equivalent(x$x, y$x, tolerance = tolerance) &
        is_equivalent(x$y, y$y, tolerance = tolerance) &
        is_equivalent(x$z, y$z, tolerance = tolerance)
}

#' @rdname is_equivalent
#' @export
is_equivalent.Point1D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
    if (!is_point1d(y)) {
        y <- as_point1d(y)
    }
    stopifnot(!any(is_degenerate(x)),
              !any(is_degenerate(y)))
    n <- max(length(x), length(y))
    x <- rep(standardize(x), length.out = n)
    y <- rep(standardize(y), length.out = n)
    is_equivalent(x$a, y$a, tolerance = tolerance) &
        is_equivalent(x$b, y$b, tolerance = tolerance)
}

#' @rdname is_equivalent
#' @export
is_equivalent.Line2D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
    if (!is_line2d(y)) {
        y <- as_line2d(y)
    }
    stopifnot(!any(is_degenerate(x)),
              !any(is_degenerate(y)))
    n <- max(length(x), length(y))
    x <- rep(standardize(x), length.out = n)
    y <- rep(standardize(y), length.out = n)
    is_equivalent(x$a, y$a, tolerance = tolerance) &
        is_equivalent(x$b, y$b, tolerance = tolerance) &
        is_equivalent(x$c, y$c, tolerance = tolerance)
}

#' @rdname is_equivalent
#' @export
is_equivalent.Plane3D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
    if (!is_plane3d(y)) {
        y <- as_plane3d(y)
    }
    stopifnot(!any(is_degenerate(x)),
              !any(is_degenerate(y)))
    n <- max(length(x), length(y))
    x <- rep(standardize(x), length.out = n)
    y <- rep(standardize(y), length.out = n)
    is_equivalent(x$a, y$a, tolerance = tolerance) &
        is_equivalent(x$b, y$b, tolerance = tolerance) &
        is_equivalent(x$c, y$c, tolerance = tolerance) &
        is_equivalent(x$d, y$d, tolerance = tolerance)
}
