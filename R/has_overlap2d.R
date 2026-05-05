#' Whether two 2D shapes overlap
#'
#' `has_overlap2d()` is an S3 generic that returns whether two 2D shapes
#' have a non-zero-area overlap (i.e. their interiors intersect).
#'
#' For two convex shapes the function uses the
#' [Separating Axis Theorem (SAT)](https://en.wikipedia.org/wiki/Hyperplane_separation_theorem).
#' For concave [Polygon2D] objects the function first checks whether the
#' axis-aligned bounding box and convex hull overlap; if neither rules out
#' overlap it returns `NA` with a warning because exact detection is not yet
#' supported.
#' For non-circular [Ellipse2D] objects the function approximates the ellipse
#' with inner and outer polygons: if the outer polygon does not overlap the
#' result is `FALSE`; if the inner polygon overlaps the result is `TRUE`;
#' otherwise `NA` is returned with a warning.
#'
#' @param x,y The two shapes to check.  Supported classes are [Polygon2D]
#'   and [Ellipse2D].
#' @param ... Ignored; only included for S3 method consistency.
#' @param n Number of vertices used to approximate non-circular [Ellipse2D]
#'   objects (default `64L`).  Larger values give tighter bounds.
#' @param tol Numeric tolerance for overlap comparisons (default `0`).
#'   A positive value requires projections to overlap by more than `tol`
#'   before overlap is declared, which avoids false positives caused by
#'   floating-point error accumulating in rotated coordinates.
#'   `sqrt(.Machine$double.eps)` is a reasonable choice when shapes are
#'   constructed via trigonometric transformations.
#' @return A logical vector (or `NA`) of length equal to the longer of `x`
#'   and `y` (for [Ellipse2D] objects; [Polygon2D] objects are always scalar).
#' @examples
#' # Two overlapping squares
#' sq1 <- as_polygon2d(as_coord2d(
#'   x = c(0, 1, 1, 0),
#'   y = c(0, 0, 1, 1)
#' ))
#' sq2 <- as_polygon2d(as_coord2d(
#'   x = c(0.5, 1.5, 1.5, 0.5),
#'   y = c(0.5, 0.5, 1.5, 1.5)
#' ))
#' sq3 <- as_polygon2d(as_coord2d(
#'   x = c(2, 3, 3, 2),
#'   y = c(2, 2, 3, 3)
#' ))
#' has_overlap2d(sq1, sq2)
#' has_overlap2d(sq1, sq3)
#'
#' # Circle vs polygon
#' circ <- as_ellipse2d(as_coord2d(0.5, 0.5), rx = 0.4)
#' has_overlap2d(sq1, circ)
#' has_overlap2d(sq3, circ)
#'
#' # Two circles
#' c1 <- as_ellipse2d(as_coord2d(0, 0), rx = 1)
#' c2 <- as_ellipse2d(as_coord2d(1.5, 0), rx = 1)
#' c3 <- as_ellipse2d(as_coord2d(3, 0), rx = 1)
#' has_overlap2d(c1, c2)
#' has_overlap2d(c1, c3)
#' @export
has_overlap2d <- function(x, y, ...) {
	UseMethod("has_overlap2d")
}

#' @rdname has_overlap2d
#' @export
has_overlap2d.Polygon2D <- function(x, y, ..., n = 64L, tol = sqrt(.Machine$double.eps)) {
	chkDots(...)
	if (is_polygon2d(y)) {
		polygon_polygon_overlap(x, y, tol = tol)
	} else if (is_ellipse2d(y)) {
		ne <- length(y$rx)
		vapply(
			seq_len(ne),
			function(i) ellipse_overlap_with_polygon(y[i], x, n = n, tol = tol),
			logical(1L)
		)
	} else {
		stop(paste(
			"Don't know how to check overlap with",
			dQuote(class(y)[1L])
		))
	}
}

#' @rdname has_overlap2d
#' @export
has_overlap2d.Ellipse2D <- function(x, y, ..., n = 64L, tol = sqrt(.Machine$double.eps)) {
	chkDots(...)
	nx <- length(x$rx)
	if (is_ellipse2d(y)) {
		ny <- length(y$rx)
		nout <- max(nx, ny)
		xr <- rep_len(seq_len(nx), nout)
		yr <- rep_len(seq_len(ny), nout)
		vapply(
			seq_len(nout),
			function(i) ellipse_ellipse_overlap(x[xr[i]], y[yr[i]], n = n, tol = tol),
			logical(1L)
		)
	} else if (is_polygon2d(y)) {
		vapply(
			seq_len(nx),
			function(i) ellipse_overlap_with_polygon(x[i], y, n = n, tol = tol),
			logical(1L)
		)
	} else {
		stop(paste(
			"Don't know how to check overlap with",
			dQuote(class(y)[1L])
		))
	}
}

# Internal: do two [min, max] ranges overlap by more than tol?
ranges_overlap_strictly <- function(r1, r2, tol = sqrt(.Machine$double.eps)) {
	r1[2L] > r2[1L] + tol && r2[2L] > r1[1L] + tol
}

# Internal: SAT for two convex polygons
sat_convex_polygon_polygon <- function(p1, p2, tol = sqrt(.Machine$double.eps)) {
	axes <- c(p1$normals, p2$normals)
	n <- length(axes)
	for (i in seq_len(n)) {
		ax <- axes[i]
		if (
			!ranges_overlap_strictly(
				range(p1 * ax),
				range(p2 * ax),
				tol = tol
			)
		) {
			return(FALSE)
		}
	}
	TRUE
}

# Internal: SAT for a convex polygon and a single circle
# center: Coord2D of length 1; radius: numeric scalar
sat_convex_polygon_circle <- function(p, center, radius, tol = sqrt(.Machine$double.eps)) {
	# Test polygon edge normals
	poly_axes <- p$normals
	n_edges <- length(poly_axes)
	for (i in seq_len(n_edges)) {
		ax <- poly_axes[i]
		cp <- center * ax
		if (
			!ranges_overlap_strictly(
				range(p * ax),
				c(cp - radius, cp + radius),
				tol = tol
			)
		) {
			return(FALSE)
		}
	}
	# Test vertex-to-center axes (direction from each vertex toward the center)
	n_verts <- length(p)
	for (i in seq_len(n_verts)) {
		# vector from vertex[i] to center
		vc <- center - p[i]
		len <- abs(vc)
		if (len < .Machine$double.eps) {
			return(TRUE) # center coincides with vertex → overlap
		}
		vc$scale(1 / len) # normalize in place (vc is a temporary clone)
		cp <- center * vc
		if (
			!ranges_overlap_strictly(
				range(p * vc),
				c(cp - radius, cp + radius),
				tol = tol
			)
		) {
			return(FALSE)
		}
	}
	TRUE
}

# Internal: do axis-aligned bounding boxes of two polygons overlap?
bbox_overlap_pp <- function(p1, p2, tol = sqrt(.Machine$double.eps)) {
	ranges_overlap_strictly(range(p1$x), range(p2$x), tol = tol) &&
		ranges_overlap_strictly(range(p1$y), range(p2$y), tol = tol)
}

# Internal: does a polygon's bbox overlap with a circle's bbox?
bbox_overlap_pc <- function(p, center, radius, tol = sqrt(.Machine$double.eps)) {
	cx <- center$x
	cy <- center$y
	ranges_overlap_strictly(
		range(p$x),
		c(cx - radius, cx + radius),
		tol = tol
	) &&
		ranges_overlap_strictly(
			range(p$y),
			c(cy - radius, cy + radius),
			tol = tol
		)
}

# Internal: polygon vs polygon (handles concave via hull fallback)
polygon_polygon_overlap <- function(p1, p2, tol = sqrt(.Machine$double.eps)) {
	if (isTRUE(p1$is_convex) && isTRUE(p2$is_convex)) {
		return(sat_convex_polygon_polygon(p1, p2, tol = tol))
	}
	# At least one is concave or unknown: conservative bbox + hull checks
	if (!bbox_overlap_pp(p1, p2, tol = tol)) {
		return(FALSE)
	}
	h1 <- if (isTRUE(p1$is_convex)) p1 else p1$convex_hull
	h2 <- if (isTRUE(p2$is_convex)) p2 else p2$convex_hull
	if (!sat_convex_polygon_polygon(h1, h2, tol = tol)) {
		return(FALSE)
	}
	warning(
		"Exact overlap detection is not yet supported for concave polygons; ",
		"their convex hulls overlap."
	)
	NA
}

# Internal: polygon vs single circle (handles concave via hull fallback)
polygon_circle_overlap <- function(p, center, radius, tol = sqrt(.Machine$double.eps)) {
	if (isTRUE(p$is_convex)) {
		return(sat_convex_polygon_circle(p, center, radius, tol = tol))
	}
	# Concave or unknown: conservative bbox + hull checks
	if (!bbox_overlap_pc(p, center, radius, tol = tol)) {
		return(FALSE)
	}
	h <- p$convex_hull
	if (!sat_convex_polygon_circle(h, center, radius, tol = tol)) {
		return(FALSE)
	}
	warning(
		"Exact overlap detection is not yet supported for concave polygons; ",
		"the convex hull overlaps with the circle."
	)
	NA
}

# Internal: single Ellipse2D (length 1) vs Polygon2D
ellipse_overlap_with_polygon <- function(e, p, n, tol = sqrt(.Machine$double.eps)) {
	if (e$is_circle) {
		center <- as_coord2d(e$x, e$y)
		return(polygon_circle_overlap(p, center, e$rx, tol = tol))
	}
	ellipse_bracket_overlap(e, p, n, tol = tol)
}

# Internal: single Ellipse2D vs single Ellipse2D
ellipse_ellipse_overlap <- function(e1, e2, n, tol = sqrt(.Machine$double.eps)) {
	if (e1$is_circle && e2$is_circle) {
		center1 <- as_coord2d(e1$x, e1$y)
		center2 <- as_coord2d(e2$x, e2$y)
		return(distance2d(center1, center2) < e1$rx + e2$rx - tol)
	}
	# Use the non-circle ellipse as the one we approximate with polygons
	if (!e1$is_circle) {
		ellipse_bracket_overlap(e1, e2, n, tol = tol)
	} else {
		ellipse_bracket_overlap(e2, e1, n, tol = tol)
	}
}

# Internal: bracket overlap of a single non-circle Ellipse2D vs another shape
# (Polygon2D or Ellipse2D) using inner/outer polygon approximations.
ellipse_bracket_overlap <- function(e, other, n, tol = sqrt(.Machine$double.eps)) {
	outer_poly <- as_polygon2d(e, n = n, type = "outer")
	outer_res <- has_overlap2d(outer_poly, other, n = n, tol = tol)
	if (!isTRUE(outer_res)) {
		return(FALSE)
	}
	inner_poly <- as_polygon2d(e, n = n, type = "inner")
	inner_res <- has_overlap2d(inner_poly, other, n = n, tol = tol)
	if (isTRUE(inner_res)) {
		return(TRUE)
	}
	warning(
		"Exact overlap detection is not yet supported for non-circular ellipses; ",
		"the result is in the boundary region between the inner and outer polygon approximations. ",
		"Increase `n` for a tighter approximation."
	)
	NA
}
