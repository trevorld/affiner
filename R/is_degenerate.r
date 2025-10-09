not_finite <- Negate(is.finite) # also tests if NA or NaN

is_degenerate <- function(x) {
	UseMethod("is_degenerate")
}

#' @export
is_degenerate.Point1D <- function(x) {
	not_finite(x) | is_congruent(x$a, 0)
}

#' @export
is_degenerate.Line2D <- function(x) {
	not_finite(x) | (is_congruent(x$a, 0) & is_congruent(x$b, 0))
}

#' @export
is_degenerate.Plane3D <- function(x) {
	not_finite(x) | (is_congruent(x$a, 0) & is_congruent(x$b, 0) & is_congruent(x$c, 0))
}

#' @export
is_degenerate.Coord1D <- function(x) {
	not_finite(x)
}

#' @export
is_degenerate.Coord2D <- function(x) {
	not_finite(x)
}

#' @export
is_degenerate.Coord3D <- function(x) {
	not_finite(x)
}
