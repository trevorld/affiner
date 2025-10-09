standardize <- function(x) {
	UseMethod("standardize")
}

# x + d = 0
#' @export
standardize.Point1D <- function(x) {
	as_point1d(a = 1, b = x$b / x$a)
}

# a * x + y + c = 0
# x + c = 0
#' @export
standardize.Line2D <- function(x) {
	a <- ifelse(is_congruent(x$b, 0), 1, x$a / x$b)
	b <- ifelse(is_congruent(x$b, 0), 0, 1)
	c <- ifelse(is_congruent(x$b, 0), x$c / x$a, x$c / x$b)
	as_line2d(a, b, c)
}

# a * x + b * y + z + d = 0
# a * x + y + d = 0
# x + d = 0
#' @export
standardize.Plane3D <- function(x) {
	a <- ifelse(is_congruent(x$c, 0), ifelse(is_congruent(x$b, 0), 1, x$a / x$b), x$a / x$c)
	b <- ifelse(is_congruent(x$c, 0), ifelse(is_congruent(x$b, 0), 0, 1), x$b / x$c)
	c <- ifelse(is_congruent(x$c, 0), 0, 1)
	d <- ifelse(is_congruent(x$c, 0), ifelse(is_congruent(x$b, 0), x$d / a, x$d / b), x$d / x$c)
	as_plane3d(a, b, c, d)
}
