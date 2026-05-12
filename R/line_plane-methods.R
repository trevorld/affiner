#' @export
length.Point1D <- function(x) {
	length(x$a)
}

#' @export
length.Line2D <- function(x) {
	length(x$a)
}

#' @export
length.Plane3D <- function(x) {
	length(x$a)
}

#' @export
`[.Point1D` <- function(x, i) {
	Point1D$new(x$a[i], x$b[i])
}

#' @export
`[.Line2D` <- function(x, i) {
	Line2D$new(x$a[i], x$b[i], x$c[i])
}

#' @export
`[.Plane3D` <- function(x, i) {
	Plane3D$new(x$a[i], x$b[i], x$c[i], x$d[i])
}

#' @export
as.data.frame.Point1D <- function(x, ...) {
	chkDots(...)
	data.frame(a = x$a, b = x$b)
}

#' @export
as.data.frame.Line2D <- function(x, ...) {
	chkDots(...)
	data.frame(a = x$a, b = x$b, c = x$c)
}

#' @export
as.data.frame.Plane3D <- function(x, ...) {
	chkDots(...)
	data.frame(a = x$a, b = x$b, c = x$c, d = x$d)
}

#' @export
as.list.Point1D <- function(x, ...) {
	chkDots(...)
	list(a = x$a, b = x$b)
}

#' @export
as.list.Line2D <- function(x, ...) {
	chkDots(...)
	list(a = x$a, b = x$b, c = x$c)
}

#' @export
as.list.Plane3D <- function(x, ...) {
	chkDots(...)
	list(a = x$a, b = x$b, c = x$c, d = x$d)
}

#' @export
rep.Point1D <- function(x, ..., length.out = NA_integer_) {
	if (isTRUE(length(x) == length.out)) {
		return(x)
	}
	id <- rep(seq.int(length(x)), ..., length.out = length.out)
	Point1D$new(x$a[id], x$b[id])
}

#' @export
rep.Line2D <- function(x, ..., length.out = NA_integer_) {
	if (isTRUE(length(x) == length.out)) {
		return(x)
	}
	id <- rep(seq.int(length(x)), ..., length.out = length.out)
	Line2D$new(x$a[id], x$b[id], x$c[id])
}

#' @export
rep.Plane3D <- function(x, ..., length.out = NA_integer_) {
	if (isTRUE(length(x) == length.out)) {
		return(x)
	}
	id <- rep(seq.int(length(x)), ..., length.out = length.out)
	Plane3D$new(x$a[id], x$b[id], x$c[id], x$d[id])
}

#' @export
c.Point1D <- function(...) {
	l <- list(...)
	stopifnot(all(vapply(l, is_point1d, logical(1))))
	a <- unlist(lapply(l, function(x) x$a))
	b <- unlist(lapply(l, function(x) x$b))
	Point1D$new(a, b)
}

#' @export
c.Line2D <- function(...) {
	l <- list(...)
	stopifnot(all(vapply(l, is_line2d, logical(1))))
	a <- unlist(lapply(l, function(x) x$a))
	b <- unlist(lapply(l, function(x) x$b))
	c <- unlist(lapply(l, function(x) x$c))
	Line2D$new(a, b, c)
}

#' @export
c.Plane3D <- function(...) {
	l <- list(...)
	stopifnot(all(vapply(l, is_plane3d, logical(1))))
	a <- unlist(lapply(l, function(x) x$a))
	b <- unlist(lapply(l, function(x) x$b))
	c <- unlist(lapply(l, function(x) x$c))
	d <- unlist(lapply(l, function(x) x$d))
	Plane3D$new(a, b, c, d)
}

#' @export
is.na.Point1D <- function(x) is.na(x$a) | is.na(x$b)

#' @export
is.na.Line2D <- function(x) is.na(x$a) | is.na(x$b) | is.na(x$c)

#' @export
is.na.Plane3D <- function(x) is.na(x$a) | is.na(x$b) | is.na(x$c) | is.na(x$d)

#' @export
is.nan.Point1D <- function(x) is.nan(x$a) | is.nan(x$b)

#' @export
is.nan.Line2D <- function(x) is.nan(x$a) | is.nan(x$b) | is.nan(x$c)

#' @export
is.nan.Plane3D <- function(x) is.nan(x$a) | is.nan(x$b) | is.nan(x$c) | is.nan(x$d)

#' @export
is.finite.Point1D <- function(x) is.finite(x$a) & is.finite(x$b)

#' @export
is.finite.Line2D <- function(x) is.finite(x$a) & is.finite(x$b) & is.finite(x$c)

#' @export
is.finite.Plane3D <- function(x) is.finite(x$a) & is.finite(x$b) & is.finite(x$c) & is.finite(x$d)

#' @export
is.infinite.Point1D <- function(x) is.infinite(x$a) | is.infinite(x$b)

#' @export
is.infinite.Line2D <- function(x) is.infinite(x$a) | is.infinite(x$b) | is.infinite(x$c)

#' @export
is.infinite.Plane3D <- function(x) {
	is.infinite(x$a) | is.infinite(x$b) | is.infinite(x$c) | is.infinite(x$d)
}

#' @rdname bounding_ranges
#' @export
range.Point1D <- function(..., na.rm = FALSE) {
	l <- list(...)
	x <- unlist(lapply(l, function(p) -p$b / p$a))
	as_coord1d(range(x, na.rm = na.rm))
}

#' @rdname bounding_ranges
#' @export
range.Line2D <- function(..., na.rm = FALSE) {
	l <- list(...)
	a <- unlist(lapply(l, function(x) x$a))
	b <- unlist(lapply(l, function(x) x$b))
	cv <- unlist(lapply(l, function(x) x$c))
	if (na.rm) {
		keep <- !is.na(a) & !is.na(b) & !is.na(cv)
		a <- a[keep]
		b <- b[keep]
		cv <- cv[keep]
	}
	x_range <- if (any(b != 0)) c(-Inf, Inf) else range(-cv / a)
	y_range <- if (any(a != 0)) c(-Inf, Inf) else range(-cv / b)
	as_coord2d(x_range, y_range)
}

#' @rdname bounding_ranges
#' @export
range.Plane3D <- function(..., na.rm = FALSE) {
	l <- list(...)
	a <- unlist(lapply(l, function(x) x$a))
	b <- unlist(lapply(l, function(x) x$b))
	cv <- unlist(lapply(l, function(x) x$c))
	d <- unlist(lapply(l, function(x) x$d))
	if (na.rm) {
		keep <- !is.na(a) & !is.na(b) & !is.na(cv) & !is.na(d)
		a <- a[keep]
		b <- b[keep]
		cv <- cv[keep]
		d <- d[keep]
	}
	x_range <- if (any(b != 0 | cv != 0)) c(-Inf, Inf) else range(-d / a)
	y_range <- if (any(a != 0 | cv != 0)) c(-Inf, Inf) else range(-d / b)
	z_range <- if (any(a != 0 | b != 0)) c(-Inf, Inf) else range(-d / cv)
	as_coord3d(x_range, y_range, z_range)
}

# "equality" is tricky unless we "normalize" the lines
# `==.Line2D`
# `==.Plane3D`
# `!=.Line2D`
# `!=.Plane3D`
