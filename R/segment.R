#' 2D line segment R6 Class
#'
#' `Segment2D` is an [R6::R6Class()] object representing a vector of two-dimensional line segments.
#' It inherits from [Coord2D] using the first endpoint `p1` as the coordinate data, so all transformation methods (e.g. `$rotate()`, `$translate()`) and arithmetic operators work directly on the segment vector.
#' The full edge vector to the second endpoint is stored privately and kept consistent under every transformation.
#'
#' @examples
#' p1 <- as_coord2d(x = c(0, 1), y = c(0, 0))
#' p2 <- as_coord2d(x = c(1, 1), y = c(0, 1))
#' s <- as_segment2d(p1, p2 = p2)
#' print(s)
#' s$p1
#' s$p2
#' s$mid_point
#' length(s)
#' s[1]
#' @export
Segment2D <- R6Class(
	"Segment2D",
	inherit = Coord2D,
	public = list(
		#' @param xyw A matrix with three columns for homogeneous `p1` coordinates.
		#'   Column names should be `"x"`, `"y"`, and `"w"`.
		#' @param vec A two-column matrix of edge vectors `p2 - p1`.
		#'   Column names should be `"x"` and `"y"`.
		initialize = function(xyw, vec) {
			super$initialize(xyw)
			private$vec <- vec
		},
		#' @param n Number of segments to print.  If `NULL` print all.
		#' @param ... Passed to [format.default()].
		print = function(n = NULL, ...) {
			ns <- nrow(private$mat_xyw)
			if (is.null(n) || n > ns) {
				n <- ns
			}
			cat("<Segment2D[", ns, "]>\n", sep = "")
			if (n > 0) {
				xyw <- self$xyw
				m <- cbind(
					"p1$x" = xyw[, 1L],
					"p1$y" = xyw[, 2L],
					"p2$x" = xyw[, 1L] + private$vec[, 1L],
					"p2$y" = xyw[, 2L] + private$vec[, 2L]
				)
				print_mat(m[1:n, , drop = FALSE], ...)
			}
			invisible(self)
		},
		#' @param mat `r r2i_transform2d_mat`
		transform = function(mat = transform2d()) {
			if (!is_transform2d(mat)) {
				mat <- as_transform2d(mat)
			}
			private$mid_cache <- NULL
			private$vec <- private$vec %*% mat[1:2, 1:2]
			super$transform(mat)
		}
	),
	private = list(
		vec = NULL,
		mid_cache = NULL
	),
	active = list(
		#' @field p1 A [Coord2D] object of the first endpoints.
		p1 = function() {
			Coord2D$new(self$xyw)
		},
		#' @field p2 A [Coord2D] object of the second endpoints.
		p2 = function() {
			xyw <- self$xyw
			Coord2D$new(cbind(
				x = xyw[, 1L] + private$vec[, 1L],
				y = xyw[, 2L] + private$vec[, 2L],
				w = xyw[, 3L]
			))
		},
		#' @field mid_point A [Coord2D] object of the segment midpoints
		#'   (computed on demand and cached until the next transformation).
		mid_point = function() {
			if (!is.null(private$mid_cache)) {
				return(private$mid_cache)
			}
			xyw <- self$xyw
			m <- Coord2D$new(cbind(
				x = xyw[, 1L] + private$vec[, 1L] / 2,
				y = xyw[, 2L] + private$vec[, 2L] / 2,
				w = xyw[, 3L]
			))
			private$mid_cache <- m
			m
		}
	)
)

#' Cast to Segment2D object
#'
#' `as_segment2d()` creates a [Segment2D] object.
#'
#' @param p1 A [Coord2D] object of first endpoints.
#' @param ... Ignored.
#' @param p2 A [Coord2D] object of second endpoints.
#'   If missing, `vec` must be supplied.
#' @param vec A [Coord2D] object of edge vectors (`p2 - p1`).
#'   Used only when `p2` is missing.
#' @return A [Segment2D] object.
#' @examples
#' p1 <- as_coord2d(x = c(0, 1), y = c(0, 0))
#' p2 <- as_coord2d(x = c(1, 1), y = c(0, 1))
#' s <- as_segment2d(p1, p2 = p2)
#' s$p1
#' s$p2
#' s$mid_point
#'
#' # From a polygon's edges
#' poly <- as_polygon2d(as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)))
#' as_segment2d(poly)
#' @export
as_segment2d <- function(...) {
	UseMethod("as_segment2d")
}

#' @rdname as_segment2d
#' @export
as_segment2d.Coord2D <- function(p1, ..., p2, vec) {
	chkDots(...)
	xyw1 <- p1$xyw
	if (missing(p2)) {
		if (!is_coord2d(vec)) {
			vec <- as_coord2d(vec)
		}
		stopifnot(length(p1) == length(vec))
		v <- cbind(x = vec$x, y = vec$y)
	} else {
		if (!is_coord2d(p2)) {
			p2 <- as_coord2d(p2)
		}
		stopifnot(length(p1) == length(p2))
		xyw2 <- p2$xyw
		v <- cbind(
			x = xyw2[, 1L] - xyw1[, 1L],
			y = xyw2[, 2L] - xyw1[, 2L]
		)
	}
	Segment2D$new(xyw1, v)
}

#' @rdname as_segment2d
#' @param p A [Polygon2D] object whose edges should be returned.
#' @export
as_segment2d.Polygon2D <- function(p, ...) {
	chkDots(...)
	p$edges
}

#' Sort segments by painter's depth
#'
#' `sort.Segment2D()` sorts a [Segment2D] vector by painter's depth at each
#' segment's midpoint (see [painter_depth()]).
#' The default order (farthest first) is the draw order required by the
#' painter's algorithm.
#'
#' @param x A [Segment2D] object.
#' @param decreasing If `TRUE` sort closest first instead of farthest first.
#' @param ... Passed to [painter_depth.Segment2D()].
#' @param scale,alpha Oblique projection parameters passed to [painter_depth.Segment2D()].
#'   Defaults to `scale = 1` rather than `0` because `scale = 0` yields zero depth
#'   for all `Coord2D` points (z is always 0), making sorting impossible.
#'   All positive `scale` values produce the same ordering, so `1` is used to
#'   avoid floating-point attenuation from small values.
#' @return A [Segment2D] object sorted by painter's depth.
#' @examples
#' p1 <- as_coord2d(x = c(0, 2, 1), y = c(0, 0, 2))
#' p2 <- as_coord2d(x = c(2, 1, 0), y = c(0, 2, 0))
#' s <- as_segment2d(p1, p2 = p2)
#' sort(s, alpha = degrees(45))
#' @export
sort.Segment2D <- function(x, decreasing = FALSE, ..., scale = 1, alpha = angle(45, "degrees")) {
	depths <- painter_depth(x, ..., scale = scale, alpha = alpha)
	x[order(depths, decreasing = decreasing)]
}
