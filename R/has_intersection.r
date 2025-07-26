#' Whether two objects intersect
#'
#' `has_intersection()` is an S3 method that returns whether two objects intersect.
#'
#' `affiner::has_intersection()` has the same S3 signature and default method as
#' `euclid::has_intersection()` (so it shouldn't matter if one masks the other).
#' @examples
#' line1 <- as_line2d("x-axis")
#' line2 <- as_line2d("y-axis")
#' line3 <- as_line2d(a = 0, b = 1, c = 2) # y + 2 = 0
#' has_intersection(line1, line1)
#' has_intersection(line1, line2)
#' has_intersection(line1, line3)
#' @param x,y The two objects to check if they intersect.
#' @param ... Passed to other methods (or ignored).
#' @return A logical vector.
#' @export
has_intersection <- function(x, y, ...) {
    UseMethod("has_intersection")
}

#' @rdname has_intersection
#' @param tolerance Numerics with differences smaller
#'                  than `tolerance` will be considered \dQuote{equivalent}.
#' @export
has_intersection.default <- function(x, y, ...) {
  !vapply(intersection(x, y), is.null, logical(1L))
}

#' @rdname has_intersection
#' @export
has_intersection.Point1D <- function(x, y, ...,
                                    tolerance = sqrt(.Machine$double.eps)) {
    if (!is_point1d(y)) {
        y <- as_point1d(y)
    }
    stopifnot(!any(is_degenerate(x)),
              !any(is_degenerate(y)))
    n <- max(length(x), length(y))
    x <- rep(standardize(x), length.out = n)
    y <- rep(standardize(y), length.out = n)
    is_equivalent(x, y, tolerance = tolerance)
}

#' @rdname has_intersection
#' @param tolerance Numerics with differences smaller
#'                  than `tolerance` will be considered \dQuote{equivalent}.
#' @export
has_intersection.Line2D <- function(x, y, ...,
                                    tolerance = sqrt(.Machine$double.eps)) {
    if (!is_line2d(y)) {
        y <- as_line2d(y)
    }
    stopifnot(!any(is_degenerate(x)),
              !any(is_degenerate(y)))
    n <- max(length(x), length(y))
    x <- rep(standardize(x), length.out = n)
    y <- rep(standardize(y), length.out = n)
    is_equivalent(x, y, tolerance = tolerance) |
        !is_parallel(x, y, tolerance = tolerance)
}

#' @rdname has_intersection
#' @export
has_intersection.Plane3D <- function(x, y, ...,
                                     tolerance = sqrt(.Machine$double.eps)) {
    if (!is_plane3d(y)) {
        y <- as_plane3d(y)
    }
    stopifnot(!any(is_degenerate(x)),
              !any(is_degenerate(y)))
    n <- max(length(x), length(y))
    x <- rep(standardize(x), length.out = n)
    y <- rep(standardize(y), length.out = n)
    is_equivalent(x, y, tolerance = tolerance) |
        !is_parallel(x, y, tolerance = tolerance)
}
