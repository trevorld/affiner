#' Whether two objects are parallel
#'
#' `is_parallel()` is a S3 method that tests whether two objects are parallel.
#'
#' @param x,y The two objects to compute if they are parallel.
#' @param ... Passed to other methods (or ignored).
#' @return A logical vector.
#' @examples
#' line1 <- as_line2d("x-axis")
#' line2 <- as_line2d("y-axis")
#' line3 <- as_line2d(a = 0, b = 1, c = 2) # y + 2 = 0
#' is_parallel(line1, line1)
#' is_parallel(line1, line2)
#' is_parallel(line1, line3)
#' @export
is_parallel <- function(x, y, ...) {
	UseMethod("is_parallel")
}

#' @rdname is_parallel
#' @param tolerance Numerics with differences smaller
#'                  than `tolerance` will be considered \dQuote{equivalent}.
#' @export
is_parallel.Line2D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	if (!is_line2d(y)) {
		y <- as_line2d(y)
	}
	stopifnot(!any(is_degenerate(x)), !any(is_degenerate(y)))
	n <- max(length(x), length(y))
	x <- rep(standardize(x), length.out = n)
	y <- rep(standardize(y), length.out = n)
	is_equivalent(x$a, y$a, tolerance = tolerance) &
		is_equivalent(x$b, y$b, tolerance = tolerance)
}

#' @rdname is_parallel
#' @export
is_parallel.Plane3D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	if (!is_plane3d(y)) {
		y <- as_plane3d(y)
	}
	stopifnot(!any(is_degenerate(x)), !any(is_degenerate(y)))
	n <- max(length(x), length(y))
	x <- rep(standardize(x), length.out = n)
	y <- rep(standardize(y), length.out = n)
	is_equivalent(x$a, y$a, tolerance = tolerance) &
		is_equivalent(x$b, y$b, tolerance = tolerance) &
		is_equivalent(x$c, y$c, tolerance = tolerance)
}
