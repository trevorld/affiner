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
#' @examples
#' line1 <- as_line2d("x-axis")
#' line2 <- as_line2d("y-axis")
#' line3 <- as_line2d(a = 0, b = 1, c = 2) # y + 2 = 0
#' intersection(line1, line1)
#' intersection(line1, line2)
#' intersection(line1, line3)
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
