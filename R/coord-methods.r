#' @export
`[.Coord1D` <- function(x, i) {
	Coord1D$new(x$xw[i, , drop = FALSE])
}

#' @export
`[.Coord2D` <- function(x, i) {
	Coord2D$new(x$xyw[i, , drop = FALSE])
}

#' @export
`[.Coord3D` <- function(x, i) {
	Coord3D$new(x$xyzw[i, , drop = FALSE])
}

#' @export
as.complex.Coord2D <- function(x, ...) {
	complex(real = x$x, imaginary = x$y)
}

#' @export
as.data.frame.Coord1D <- function(x, ...) {
	as.data.frame(x$xw)
}

#' @export
as.data.frame.Coord2D <- function(x, ...) {
	as.data.frame(x$xyw)
}

#' @export
as.data.frame.Coord3D <- function(x, ...) {
	as.data.frame(x$xyzw)
}

#' @export
as.list.Coord1D <- function(x, ...) {
	as.list(as.data.frame(x$xw))
}

#' @export
as.list.Coord2D <- function(x, ...) {
	as.list(as.data.frame(x$xyw))
}

#' @export
as.list.Coord3D <- function(x, ...) {
	as.list(as.data.frame(x$xyzw))
}

#' @export
as.matrix.Coord1D <- function(x, ...) {
	x$xw
}

#' @export
as.matrix.Coord2D <- function(x, ...) {
	x$xyw
}

#' @export
as.matrix.Coord3D <- function(x, ...) {
	x$xyzw
}

#' @export
c.Coord1D <- function(...) {
	l <- list(...)
	stopifnot(all(vapply(l, is_coord1d, logical(1))))
	m <- do.call(rbind, lapply(l, as.matrix))
	Coord1D$new(m)
}

#' @export
c.Coord2D <- function(...) {
	l <- list(...)
	stopifnot(all(vapply(l, is_coord2d, logical(1))))
	m <- do.call(rbind, lapply(l, as.matrix))
	Coord2D$new(m)
}

#' @export
c.Coord3D <- function(...) {
	l <- list(...)
	stopifnot(all(vapply(l, is_coord3d, logical(1))))
	m <- do.call(rbind, lapply(l, as.matrix))
	Coord3D$new(m)
}

#' @export
length.Coord1D <- function(x) {
	nrow(x$.__enclos_env__$private$mat_xw)
}

#' @export
length.Coord2D <- function(x) {
	nrow(x$.__enclos_env__$private$mat_xyw)
}

#' @export
length.Coord3D <- function(x) {
	nrow(x$.__enclos_env__$private$mat_xyzw)
}

#' @export
rep.Coord1D <- function(x, ..., length.out = NA_integer_) {
	if (isTRUE(length(x) == length.out)) {
		return(x)
	}
	id <- rep(seq.int(length(x)), ..., length.out = length.out)
	Coord1D$new(x$xw[id, , drop = FALSE])
}

#' @export
rep.Coord2D <- function(x, ..., length.out = NA_integer_) {
	if (isTRUE(length(x) == length.out)) {
		return(x)
	}
	id <- rep(seq.int(length(x)), ..., length.out = length.out)
	Coord2D$new(x$xyw[id, , drop = FALSE])
}

#' @export
rep.Coord3D <- function(x, ..., length.out = NA_integer_) {
	if (isTRUE(length(x) == length.out)) {
		return(x)
	}
	id <- rep(seq.int(length(x)), ..., length.out = length.out)
	Coord3D$new(x$xyzw[id, , drop = FALSE])
}

#' Compute centroids of coordinates
#'
#' `mean()`computes centroids for [Coord1D], [Coord2D], and [Coord3D] class objects
#'
#' @param x A [Coord1D], [Coord2D], or [Coord3D] object
#' @param ... Passed to [base::mean()]
#' @return A [Coord1D], [Coord2D], or [Coord3D] class object of length one
#' @examples
#' p <- as_coord2d(x = 1:4, y = 1:4)
#' print(mean(p))
#' print(sum(p) / length(p)) # less efficient alternative
#'
#' p <- as_coord3d(x = 1:4, y = 1:4, z = 1:4)
#' print(mean(p))
#' @name centroid
#' @export
mean.Coord1D <- function(x, ...) {
	as_coord1d(mean(x$x, ...))
}

#' @rdname centroid
#' @export
mean.Coord2D <- function(x, ...) {
	as_coord2d(mean(x$x, ...), mean(x$y, ...))
}

#' @rdname centroid
#' @export
mean.Coord3D <- function(x, ...) {
	as_coord3d(mean(x$x, ...), mean(x$y, ...), mean(x$z, ...))
}

#' Compute 2D convex hulls
#'
#' `convex_hull2d()` is a S3 generic for computing the convex hull of an object.
#' There is an implemented method supporting [Coord2D] class objects
#' using [grDevices::chull()] to compute the convex hull.
#'
#' @param x An object representing object to compute convex hull of such as a [Coord2D] class object.
#' @param ... Further arguments passed to or from other methods.
#' @return An object of same class as `x` representing just the subset of points on the convex hull.
#'         The method for [Coord2D] class objects returns these points in counter-clockwise order.
#' @examples
#' p <- as_coord2d(x = rnorm(25), y = rnorm(25))
#' print(convex_hull2d(p))
#'
#' # Equivalent to following caculation using `grDevices::chull()`
#' all.equal(convex_hull2d(p),
#'           p[rev(grDevices::chull(as.list(p)))])
#' @export
convex_hull2d <- function(x, ...) {
	UseMethod("convex_hull2d")
}

#' @rdname convex_hull2d
#' @export
convex_hull2d.Coord2D <- function(x, ...) {
	x[rev(grDevices::chull(as.list(x)))]
}

# Group "Summary"

#' @export
is.na.Coord1D <- function(x) is.na(x$x)

#' @export
is.na.Coord2D <- function(x) is.na(x$x) | is.na(x$y)

#' @export
is.na.Coord3D <- function(x) is.na(x$x) | is.na(x$y) | is.na(x$z)

#' @export
is.nan.Coord1D <- function(x) is.nan(x$x)

#' @export
is.nan.Coord2D <- function(x) is.nan(x$x) | is.nan(x$y)

#' @export
is.nan.Coord3D <- function(x) is.nan(x$x) | is.nan(x$y) | is.nan(x$z)

#' @export
is.finite.Coord1D <- function(x) is.finite(x$x)

#' @export
is.finite.Coord2D <- function(x) is.finite(x$x) & is.finite(x$y)

#' @export
is.finite.Coord3D <- function(x) is.finite(x$x) & is.finite(x$y) & is.finite(x$z)

#' @export
is.infinite.Coord1D <- function(x) is.infinite(x$x)

#' @export
is.infinite.Coord2D <- function(x) is.infinite(x$x) | is.infinite(x$y)

#' @export
is.infinite.Coord3D <- function(x) is.infinite(x$x) | is.infinite(x$y) | is.infinite(x$z)

#' @export
sum.Coord1D <- function(..., na.rm = FALSE) {
	l <- list(...)
	if (na.rm) {
		l <- lapply(l, function(p) p[!is.na(p)])
	}
	xs <- sum(sapply(l, function(p) sum(p$x)))
	as_coord1d(xs)
}

#' @export
sum.Coord2D <- function(..., na.rm = FALSE) {
	l <- list(...)
	if (na.rm) {
		l <- lapply(l, function(p) p[!is.na(p)])
	}
	xs <- sum(sapply(l, function(p) sum(p$x)))
	ys <- sum(sapply(l, function(p) sum(p$y)))
	as_coord2d(xs, ys)
}

#' @export
sum.Coord3D <- function(..., na.rm = FALSE) {
	l <- list(...)
	if (na.rm) {
		l <- lapply(l, function(p) p[!is.na(p)])
	}
	xs <- sum(sapply(l, function(p) sum(p$x)))
	ys <- sum(sapply(l, function(p) sum(p$y)))
	zs <- sum(sapply(l, function(p) sum(p$z)))
	as_coord3d(xs, ys, zs)
}

#' @export
`==.Coord1D` <- function(e1, e2) {
	stopifnot(is_coord1d(e1) && is_coord1d(e2))
	e1$x == e2$x
}

#' @export
`==.Coord2D` <- function(e1, e2) {
	stopifnot(is_coord2d(e1) && is_coord2d(e2))
	(e1$x == e2$x) & (e1$y == e2$y)
}

#' @export
`==.Coord3D` <- function(e1, e2) {
	stopifnot(is_coord3d(e1) && is_coord3d(e2))
	(e1$x == e2$x) & (e1$y == e2$y) & (e1$z == e2$z)
}

#' @export
`!=.Coord1D` <- function(e1, e2) {
	stopifnot(is_coord1d(e1) && is_coord1d(e2))
	e1$x != e2$x
}

#' @export
`!=.Coord2D` <- function(e1, e2) {
	stopifnot(is_coord2d(e1) && is_coord2d(e2))
	(e1$x != e2$x) | (e1$y != e2$y)
}

#' @export
`!=.Coord3D` <- function(e1, e2) {
	stopifnot(is_coord3d(e1) && is_coord3d(e2))
	(e1$x != e2$x) | (e1$y != e2$y) | (e1$z != e2$z)
}

#' @export
`+.Coord1D` <- function(e1, e2) {
	if (missing(e2)) {
		e1
	} else {
		plus_coord1d(e1, e2)
	}
}

#' @export
`+.Coord2D` <- function(e1, e2) {
	if (missing(e2)) {
		e1
	} else {
		plus_coord2d(e1, e2)
	}
}

#' @export
`+.Coord3D` <- function(e1, e2) {
	if (missing(e2)) {
		e1
	} else {
		plus_coord3d(e1, e2)
	}
}

#' @export
`-.Coord1D` <- function(e1, e2) {
	if (missing(e2)) {
		e1$clone()$scale(-1)
	} else {
		minus_coord1d(e1, e2)
	}
}

#' @export
`-.Coord2D` <- function(e1, e2) {
	if (missing(e2)) {
		e1$clone()$scale(-1)
	} else {
		minus_coord2d(e1, e2)
	}
}

#' @export
`-.Coord3D` <- function(e1, e2) {
	if (missing(e2)) {
		e1$clone()$scale(-1)
	} else {
		minus_coord3d(e1, e2)
	}
}

#' @export
`*.Coord1D` <- function(e1, e2) {
	if (is_coord1d(e1) && is_coord1d(e2)) {
		inner_coord1d(e1, e2)
	} else if (is_coord1d(e1) && is.numeric(e2)) {
		e1$clone()$scale(e2)
	} else if (is.numeric(e1) && is_coord1d(e2)) {
		e2$clone()$scale(e1)
	} else {
		stop(paste(
			"Don't know how to multiply objects of class",
			sQuote(class(e1)[1]),
			"and class",
			sQuote(class(e2)[1])
		))
	}
}

#' @export
`*.Coord2D` <- function(e1, e2) {
	if (is_coord2d(e1) && is_coord2d(e2)) {
		inner_coord2d(e1, e2)
	} else if (is_coord2d(e1) && is.numeric(e2)) {
		e1$clone()$scale(e2)
	} else if (is.numeric(e1) && is_coord2d(e2)) {
		e2$clone()$scale(e1)
	} else {
		stop(paste(
			"Don't know how to multiply objects of class",
			sQuote(class(e1)[1]),
			"and class",
			sQuote(class(e2)[1])
		))
	}
}

#' @export
`*.Coord3D` <- function(e1, e2) {
	if (is_coord3d(e1) && is_coord3d(e2)) {
		inner_coord3d(e1, e2)
	} else if (is_coord3d(e1) && is.numeric(e2)) {
		e1$clone()$scale(e2)
	} else if (is.numeric(e1) && is_coord3d(e2)) {
		e2$clone()$scale(e1)
	} else {
		stop(paste(
			"Don't know how to multiply objects of class",
			sQuote(class(e1)[1]),
			"and class",
			sQuote(class(e2)[1])
		))
	}
}

#' @export
`/.Coord1D` <- function(e1, e2) {
	if (is_coord1d(e1) && is.numeric(e2)) {
		e1$clone()$scale(1 / e2)
	} else {
		stop(paste(
			"Don't know how to divide objects of class",
			sQuote(class(e1)[1]),
			"and class",
			sQuote(class(e2)[1])
		))
	}
}

#' @export
`/.Coord2D` <- function(e1, e2) {
	if (is_coord2d(e1) && is.numeric(e2)) {
		e1$clone()$scale(1 / e2)
	} else {
		stop(paste(
			"Don't know how to divide objects of class",
			sQuote(class(e1)[1]),
			"and class",
			sQuote(class(e2)[1])
		))
	}
}

#' @export
`/.Coord3D` <- function(e1, e2) {
	if (is_coord3d(e1) && is.numeric(e2)) {
		e1$clone()$scale(1 / e2)
	} else {
		stop(paste(
			"Don't know how to divide objects of class",
			sQuote(class(e1)[1]),
			"and class",
			sQuote(class(e2)[1])
		))
	}
}

#' Compute axis-aligned ranges
#'
#' `range()` computes axis-aligned ranges for
#' [Coord1D], [Coord2D], and [Coord3D] class objects.
#' @param na.rm logical, indicating if `NA`'s should be omitted
#' @param ... [Coord1D], [Coord2D], or [Coord3D] object(s)
#' @name bounding_ranges
#' @return Either a [Coord1D], [Coord2D], or [Coord3D] object of length two.
#'         The first element will have the minimum x/y(/z) coordinates
#'         and the second element will have the maximum x/y(/z) coordinates
#'         of the axis-aligned ranges.
#' @examples
#' range(as_coord2d(rnorm(5), rnorm(5)))
#' range(as_coord3d(rnorm(5), rnorm(5), rnorm(5)))
#' @export
range.Coord1D <- function(..., na.rm = FALSE) {
	x <- c.Coord1D(...)
	if (na.rm) {
		x <- x[!is.na(x)]
	}
	as_coord1d(range(x$x))
}

#' @rdname bounding_ranges
#' @export
range.Coord2D <- function(..., na.rm = FALSE) {
	x <- c.Coord2D(...)
	if (na.rm) {
		x <- x[!is.na(x)]
	}
	as_coord2d(range(x$x), range(x$y))
}

#' @rdname bounding_ranges
#' @export
range.Coord3D <- function(..., na.rm = FALSE) {
	x <- c.Coord3D(...)
	if (na.rm) {
		x <- x[!is.na(x)]
	}
	as_coord3d(range(x$x), range(x$y), range(x$z))
}

inner_coord1d <- function(p1, p2) {
	n <- max(length(p1), length(p2))
	p1 <- rep_len(p1, n)
	p2 <- rep_len(p2, n)
	p1$x * p2$x
}

inner_coord2d <- function(p1, p2) {
	n <- max(length(p1), length(p2))
	p1 <- rep_len(p1, n)
	p2 <- rep_len(p2, n)
	rowSums(p1$xyw[, 1:2, drop = FALSE] * p2$xyw[, 1:2, drop = FALSE])
}

inner_coord3d <- function(p1, p2) {
	n <- max(length(p1), length(p2))
	p1 <- rep_len(p1, n)
	p2 <- rep_len(p2, n)
	rowSums(p1$xyzw[, 1:3, drop = FALSE] * p2$xyzw[, 1:3, drop = FALSE])
}

plus_coord1d <- function(p1, p2) {
	stopifnot(is_coord1d(p1), is_coord1d(p2))
	if (length(p1) == 1) {
		p2$clone()$translate(p1)
	} else {
		p1$clone()$translate(p2)
	}
}

plus_coord2d <- function(p1, p2) {
	stopifnot(is_coord2d(p1), is_coord2d(p2))
	if (length(p1) == 1) {
		p2$clone()$translate(p1)
	} else {
		p1$clone()$translate(p2)
	}
}

plus_coord3d <- function(p1, p2) {
	stopifnot(is_coord3d(p1), is_coord3d(p2))
	if (length(p1) == 1) {
		p2$clone()$translate(p1)
	} else {
		p1$clone()$translate(p2)
	}
}

minus_coord1d <- function(p1, p2) {
	stopifnot(is_coord1d(p1), is_coord1d(p2))
	p1$clone()$translate(p2$clone()$scale(-1))
}

minus_coord2d <- function(p1, p2) {
	stopifnot(is_coord2d(p1), is_coord2d(p2))
	p1$clone()$translate(p2$clone()$scale(-1))
}

minus_coord3d <- function(p1, p2) {
	stopifnot(is_coord3d(p1), is_coord3d(p2))
	p1$clone()$translate(p2$clone()$scale(-1))
}

# Base rounding function

#' @export
ceiling.Coord1D <- function(x) {
	as_coord1d(ceiling(x$x))
}

#' @export
ceiling.Coord2D <- function(x) {
	as_coord2d(ceiling(x$x), ceiling(x$y))
}

#' @export
ceiling.Coord3D <- function(x) {
	as_coord3d(ceiling(x$x), ceiling(x$y), ceiling(x$z))
}

#' @export
floor.Coord1D <- function(x) {
	as_coord1d(floor(x$x))
}

#' @export
floor.Coord2D <- function(x) {
	as_coord2d(floor(x$x), floor(x$y))
}

#' @export
floor.Coord3D <- function(x) {
	as_coord3d(floor(x$x), floor(x$y), floor(x$z))
}

#' @export
round.Coord1D <- function(x, digits = 0, ...) {
	as_coord1d(round(x$x, digits))
}

#' @export
round.Coord2D <- function(x, digits = 0, ...) {
	as_coord2d(round(x$x, digits), round(x$y, digits))
}

#' @export
round.Coord3D <- function(x, digits = 0, ...) {
	as_coord3d(round(x$x, digits), round(x$y, digits), round(x$z, digits))
}

#' @export
signif.Coord1D <- function(x, digits = 6) {
	as_coord1d(signif(x$x, digits))
}

#' @export
signif.Coord2D <- function(x, digits = 6) {
	as_coord2d(signif(x$x, digits), signif(x$y, digits))
}

#' @export
signif.Coord3D <- function(x, digits = 6) {
	as_coord3d(signif(x$x, digits), signif(x$y, digits), signif(x$z, digits))
}

#' @export
trunc.Coord1D <- function(x, ...) {
	as_coord1d(trunc(x$x))
}

#' @export
trunc.Coord2D <- function(x, ...) {
	as_coord2d(trunc(x$x), trunc(x$y))
}

#' @export
trunc.Coord3D <- function(x, ...) {
	as_coord3d(trunc(x$x), trunc(x$y), trunc(x$z))
}

# Group "Complex"

#' @export
Re.Coord2D <- function(z) {
	z$x
}

#' @export
Im.Coord2D <- function(z) {
	z$y
}

#' @export
Mod.Coord2D <- function(z) {
	sqrt(rowSums(z$xyw[, 1:2, drop = FALSE]^2))
}

#' @export
Conj.Coord2D <- function(z) {
	complex(real = z$x, imaginary = -z$y)
}

#' @export
Arg.Coord2D <- function(z) {
	atan2(z$y, z$x)
}

#' Compute 3D vector cross product
#'
#' `cross_product3d()` computes the cross product of two [Coord3D] class vectors.
#' @param x A [Coord3D] class vector.
#' @param y A [Coord3D] class vector.
#' @return A [Coord3D] class vector
#' @examples
#' x <- as_coord3d(2, 3, 4)
#' y <- as_coord3d(5, 6, 7)
#' cross_product3d(x, y)
#' if (getRversion() >= "4.4.0") {
#'   crossprod(x, y)
#' }
#' @export
cross_product3d <- function(x, y) {
	stopifnot(is_coord3d(x), is_coord3d(y))
	n <- max(length(x), length(y))
	x <- rep_len(x, n)
	y <- rep_len(y, n)
	m <- matrix(1, nrow = n, ncol = 4, dimnames = list(NULL, c("x", "y", "z", "w")))
	m[, 1] <- x$y * y$z - x$z * y$y
	m[, 2] <- x$z * y$x - x$x * y$z
	m[, 3] <- x$x * y$y - x$y * y$x
	Coord3D$new(m)
}

#' @rawNamespace if (getRversion() >= "4.4.0") {
#'   S3method("crossprod",Coord3D)
#' }
crossprod.Coord3D <- function(x, y, ...) cross_product3d(x, y)

# nolint start
# #' Scalar projections
# #'
# #' `as.double.Coord2D()` and `as.double.Coord3D()` computes the scalar projections of [Coord2D]
# #' and [Coord3D] vectors.
# #' The scalar projection of a vector `x` onto a vector `y` is also
# #' known as the component of `x` in the direction of `y`.
# #'
# #' @param x [Coord2D] or [Coord3D] object
# #' @param y [Coord2D] or [Coord3D] object of length one to project onto.
# #' @inheritParams project2d
# #' @param ... If `y` is not a coordinate vector passed to either [as_coord2d()] or [as_coord3d()].
# #' @examples
# #' p2 <- as_coord2d(rnorm(5), rnorm(5))
# #' as.numeric(p2)
# #' p3 <- as_coord2d(rnorm(5), rnorm(5), rnorm(5))
# #' as.numeric(p3)
# #' @name scalar_projection
# #' @return A double vector
# #' @export
# as.double.Coord2D <- function(x, y = as_coord2d(1, 0), ...) {
#     if (!is_coord2d(y))
#         y <- as_coord2d(y, ...)
#     stopifnot(length(y) == 1)
#     (x * y) / abs(y)
# }
#
# #' @rdname scalar_projection
# #' @export
# as.double.Coord3D <- function(x, y = as_coord3d(1, 0, 0), ...) {
#     if (!is_coord3d(y))
#         y <- as_coord3d(y, ...)
#     stopifnot(length(y) == 1)
#     (x * y) / abs(y)
# }
# nolint end
