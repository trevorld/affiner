#' The intersection of two objects.
#'
#' `intersection()` is an S3 method that returns the intersection of two objects.
#'
#' `affiner::intersection()` has the same S3 signature as
#' `euclid::intersection()` (so it shouldn't matter if one masks the other).
#'
#' @param x,y The two objects to compute intersection for.
#' @param ... Passed to other methods (or ignored).
#' @return A list of the object intersections (or `NULL` if no intersection).
#'   For [Line2D]-[Ellipse2D] intersections each list element is a [Coord2D]
#'   of length 2 (two crossing points), length 1 (tangent), or `NULL`
#'   (no intersection).
#' @examples
#' line1 <- as_line2d("x-axis")
#' line2 <- as_line2d("y-axis")
#' line3 <- as_line2d(a = 0, b = 1, c = 2) # y + 2 = 0
#' intersection(line1, line1)
#' intersection(line1, line2)
#' intersection(line1, line3)
#'
#' # Unit circle vs x-axis: two points at (1, 0) and (-1, 0)
#' circ <- as_ellipse2d(as_coord2d("origin"), r = 1)
#' intersection(circ, line1)
#'
#' # Tangent: circle vs line y = 1 (touches top of circle)
#' line_top <- as_line2d(a = 0, b = 1, c = -1) # y - 1 = 0
#' intersection(circ, line_top)
#'
#' # No intersection: circle vs y = 2
#' line_above <- as_line2d(a = 0, b = 1, c = -2) # y - 2 = 0
#' intersection(circ, line_above)
#' @export
intersection <- function(x, y, ...) {
	UseMethod("intersection")
}

#' @rdname intersection
#' @export
intersection.Point1D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	if (!is_point1d(y)) {
		y <- as_point1d(y)
	}
	stopifnot(!any(is_degenerate(x)), !any(is_degenerate(y)))
	n <- max(length(x), length(y))
	x <- rep(standardize(x), length.out = n)
	y <- rep(standardize(y), length.out = n)
	l <- vector("list", n)
	for (i in seq_len(n)) {
		xi <- x[i]
		yi <- y[i]
		if (is_equivalent(xi, yi, tolerance = tolerance)) {
			l[[i]] <- xi
		} # else already NULL
	}
	l
}

#' @rdname intersection
#' @param tolerance Numerics with differences smaller
#'                  than `tolerance` will be considered \dQuote{equivalent}.
#' @export
intersection.Line2D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	if (is_ellipse2d(y)) {
		return(intersection_Ellipse2D_Line2D(y, x, ..., tolerance = tolerance))
	}
	if (is_coord2d(y)) {
		return(intersection_Coord2D_Line2D(y, x, ..., tolerance = tolerance))
	}
	if (!is_line2d(y)) {
		y <- as_line2d(y)
	}
	stopifnot(!any(is_degenerate(x)), !any(is_degenerate(y)))
	n <- max(length(x), length(y))
	x <- rep(standardize(x), length.out = n)
	y <- rep(standardize(y), length.out = n)
	l <- vector("list", n)
	for (i in seq_len(n)) {
		xi <- x[i]
		yi <- y[i]
		if (is_equivalent(xi, yi, tolerance = tolerance)) {
			l[[i]] <- xi
		} else if (!is_parallel(xi, yi, tolerance = tolerance)) {
			m <- matrix(c(xi$a, xi$b, yi$a, yi$b), nrow = 2L, ncol = 2L, byrow = TRUE)
			v <- matrix(c(-xi$c, -yi$c))
			s <- solve(m, v)
			l[[i]] <- as_coord2d(x = s[1L], y = s[2L])
		} # else already NULL
	}
	l
}

intersection_Coord2D_Line2D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	stopifnot(!any(is_degenerate(x)), !any(is_degenerate(y)))
	n <- max(length(x), length(y))
	x <- rep(x, length.out = n)
	y <- rep(y, length.out = n)
	l <- vector("list", n)
	for (i in seq_len(n)) {
		xi <- x[i] # Coord2D
		yi <- y[i] # Line2D
		if (is_equivalent(yi$a * xi$x + yi$b * xi$y, -yi$c, tolerance = tolerance)) {
			l[[i]] <- xi
		} # else already NULL
	}
	l
}

#' @rdname intersection
#' @export
intersection.Plane3D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	if (is_coord3d(y)) {
		return(intersection_Coord3D_Plane3D(y, x, ..., tolerance = tolerance))
	}
	if (!is_plane3d(y)) {
		y <- as_plane3d(y)
	}
	stopifnot(!any(is_degenerate(x)), !any(is_degenerate(y)))
	n <- max(length(x), length(y))
	x <- rep(standardize(x), length.out = n)
	y <- rep(standardize(y), length.out = n)
	l <- vector("list", n)
	for (i in seq_len(n)) {
		xi <- x[i]
		yi <- y[i]
		if (is_equivalent(xi, yi, tolerance = tolerance)) {
			l[[i]] <- xi
		} else if (!is_parallel(xi, yi, tolerance = tolerance)) {
			stop("We have not implemented a `Line3D` class.")
		} # else already NULL
	}
	l
}

intersection_Coord3D_Plane3D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	stopifnot(!any(is_degenerate(x)), !any(is_degenerate(y)))
	n <- max(length(x), length(y))
	x <- rep(x, length.out = n)
	y <- rep(y, length.out = n)
	l <- vector("list", n)
	for (i in seq_len(n)) {
		xi <- x[i] # Coord3D
		yi <- y[i] # Plane3D
		if (is_equivalent(yi$a * xi$x + yi$b * xi$y + yi$c * xi$z, -yi$d, tolerance = tolerance)) {
			l[[i]] <- xi
		} # else already NULL
	}
	l
}

# Vectorised wrapper: each (e[i], l[j]) pair (recycled to max length)
intersection_Ellipse2D_Line2D <- function(e, l, ..., tolerance = sqrt(.Machine$double.eps)) {
	ne <- length(e$rx)
	nl <- length(l)
	n <- max(ne, nl)
	ei <- rep_len(seq_len(ne), n)
	li <- rep_len(seq_len(nl), n)
	result <- vector("list", n)
	for (k in seq_len(n)) {
		r <- ellipse_line_intersection_single(e[ei[k]], l[li[k]], tolerance)
		if (!is.null(r)) result[[k]] <- r
	}
	result
}

# Single Ellipse2D (length 1) vs single Line2D intersection.
# Line: a*x + b*y + c = 0
# Ellipse parametric (affiner convention, y up, CCW angle theta):
#   x(t) = cx + rx*cos(t)*cos(theta) - ry*sin(t)*sin(theta)
#   y(t) = cy + rx*cos(t)*sin(theta) + ry*sin(t)*cos(theta)
# Substituting into the line equation gives A*cos(t) + B*sin(t) + C = 0
# where A = rx*(a*cos(theta) + b*sin(theta)),
#       B = ry*(b*cos(theta) - a*sin(theta)),
#       C = a*cx + b*cy + c.
# Rewriting as R*cos(t - phi) = -C and solving for t.
ellipse_line_intersection_single <- function(e, l, tolerance = sqrt(.Machine$double.eps)) {
	cx <- e$x
	cy <- e$y
	rx <- e$rx
	ry <- e$ry
	theta <- e$theta
	ct <- cos(theta)
	st <- sin(theta)
	a <- l$a
	b <- l$b
	cc <- l$c
	A <- rx * (a * ct + b * st)
	B <- ry * (b * ct - a * st)
	C <- a * cx + b * cy + cc
	R <- sqrt(A^2 + B^2)
	if (R < tolerance) {
		return(NULL) # degenerate ellipse
	}
	ratio <- -C / R
	if (ratio < -(1 + tolerance) || ratio > (1 + tolerance)) {
		return(NULL) # no intersection
	}
	ratio <- max(-1, min(1, ratio))
	phi <- atan2(B, A)
	da <- acos(ratio)
	t1 <- phi + da
	t2 <- phi - da
	x1 <- cx + rx * cos(t1) * ct - ry * sin(t1) * st
	y1 <- cy + rx * cos(t1) * st + ry * sin(t1) * ct
	x2 <- cx + rx * cos(t2) * ct - ry * sin(t2) * st
	y2 <- cy + rx * cos(t2) * st + ry * sin(t2) * ct
	if (abs(x1 - x2) < tolerance && abs(y1 - y2) < tolerance) {
		as_coord2d(x = x1, y = y1) # tangent: one point
	} else {
		# │ condition │        consequence        │
		# │ b > 0     │ x1 < x2                   │
		# │ b < 0     │ x1 > x2                   │
		# │ b = 0     │ x1 = x2 (vertical line)   │
		# │ a < 0     │ y1 < y2                   │
		# │ a > 0     │ y1 > y2                   │
		# │ a = 0     │ y1 = y2 (horizontal line) │
		as_coord2d(x = c(x1, x2), y = c(y1, y2))
	}
}

#' @rdname intersection
#' @export
intersection.Coord1D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	if (!is_coord1d(y)) {
		y <- as_coord1d(y)
	}
	stopifnot(!any(is_degenerate(x)), !any(is_degenerate(y)))
	n <- max(length(x), length(y))
	x <- rep(x, length.out = n)
	y <- rep(y, length.out = n)
	l <- vector("list", n)
	for (i in seq_len(n)) {
		xi <- x[i]
		yi <- y[i]
		if (is_equivalent(xi, yi, tolerance = tolerance)) {
			l[[i]] <- xi
		} # else already NULL
	}
	l
}

#' @rdname intersection
#' @export
intersection.Coord2D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	if (is_line2d(y)) {
		return(intersection_Coord2D_Line2D(x, y, ..., tolerance = tolerance))
	}
	if (!is_coord2d(y)) {
		y <- as_coord2d(y)
	}
	stopifnot(!any(is_degenerate(x)), !any(is_degenerate(y)))
	n <- max(length(x), length(y))
	x <- rep(x, length.out = n)
	y <- rep(y, length.out = n)
	l <- vector("list", n)
	for (i in seq_len(n)) {
		xi <- x[i]
		yi <- y[i]
		if (is_equivalent(xi, yi, tolerance = tolerance)) {
			l[[i]] <- xi
		} # else already NULL
	}
	l
}

#' @rdname intersection
#' @export
intersection.Ellipse2D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	if (is_line2d(y)) {
		return(intersection_Ellipse2D_Line2D(x, y, ..., tolerance = tolerance))
	}
	NextMethod()
}

#' @rdname intersection
#' @export
intersection.Coord3D <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
	if (is_plane3d(y)) {
		return(intersection_Coord3D_Plane3D(x, y, ..., tolerance = tolerance))
	}
	if (!is_coord3d(y)) {
		y <- as_coord3d(y)
	}
	stopifnot(!any(is_degenerate(x)), !any(is_degenerate(y)))
	n <- max(length(x), length(y))
	x <- rep(x, length.out = n)
	y <- rep(y, length.out = n)
	l <- vector("list", n)
	for (i in seq_len(n)) {
		xi <- x[i]
		yi <- y[i]
		if (is_equivalent(xi, yi, tolerance = tolerance)) {
			l[[i]] <- xi
		} # else already NULL
	}
	l
}
