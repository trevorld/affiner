#' 2D polygon R6 Class
#'
#' `Polygon2D` is an [R6::R6Class()] object representing a two-dimensional
#' polygon.  It inherits from [Coord2D] so all transformation methods
#' (e.g. `$rotate()`, `$translate()`) and arithmetic operators work directly
#' on the polygon.
#'
#' @examples
#' vertices <- as_coord2d(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0))
#' p <- as_polygon2d(vertices)
#' print(p)
#' p$is_convex
#' p$normals
#' h <- p$convex_hull
#'
#' # Transformations are inherited from Coord2D
#' p$rotate(degrees(45))
#' @export
Polygon2D <- R6Class(
	"Polygon2D",
	inherit = Coord2D,
	public = list(
		#' @param xyw A matrix with three columns representing (homogeneous)
		#'   coordinates.  The first two columns are x/y coordinates and the
		#'   last column is all ones.  Column names should be `"x"`, `"y"`,
		#'   and `"w"`.
		#' @param convex `NA` (default) to auto-detect convexity from `xyw`,
		#'   `TRUE` to assert convex (skip detection), or `FALSE` to mark as
		#'   concave.
		initialize = function(xyw, convex = NA) {
			super$initialize(xyw)
			private$convex <- as.logical(convex)
		},
		#' @param n Number of vertices to print.  If `NULL` print all.
		#' @param ... Passed to [format.default()].
		print = function(n = NULL, ...) {
			nv <- nrow(private$mat_xyw)
			if (is.null(n) || n > nv) {
				n <- nv
			}
			convex_str <- self$is_convex
			cat(
				"<Polygon2D[",
				nv,
				" vertices, convex = ",
				convex_str,
				"]>\n",
				sep = ""
			)
			if (n > 0) {
				print_mat(self$xyw[1:n, , drop = FALSE], ...)
			}
			invisible(self)
		},
		#' @param mat `r r2i_transform1d_mat`
		transform = function(mat = transform2d()) {
			private$hull_cache <- NULL
			private$normals_cache <- NULL
			super$transform(mat)
		}
	),
	private = list(
		convex = NA,
		hull_cache = NULL,
		normals_cache = NULL
	),
	active = list(
		#' @field is_convex Logical indicating whether the polygon is convex
		#'   (`TRUE`), concave (`FALSE`), or unknown (`NA`).
		is_convex = function() {
			if (is.na(private$convex)) {
				private$convex <- is_convex_xyw(private$mat_xyw)
			}
			private$convex
		},
		#' @field convex_hull A [Polygon2D] object of the convex hull.
		#'  If the polygon is already convex (`$convex == TRUE`) it returns itself.
		#'  For concave polygons the hull is computed on demand and cached until the next transformation.
		convex_hull = function() {
			if (isTRUE(private$convex)) {
				return(invisible(self))
			}
			if (!is.null(private$hull_cache)) {
				return(private$hull_cache)
			}
			# self$x / self$y apply any pending transforms
			idx <- rev(grDevices::chull(list(x = self$x, y = self$y)))
			hull <- Polygon2D$new(private$mat_xyw[idx, , drop = FALSE], convex = TRUE)
			private$hull_cache <- hull
			hull
		},
		#' @field normals A [Coord2D] object of unit edge normals
		#  (one per edge, computed on demand, and cached until the next transformation).
		normals = function() {
			if (!is.null(private$normals_cache)) {
				return(private$normals_cache)
			}
			private$apply_any_delayed_transformations()
			xyw <- private$mat_xyw
			n <- nrow(xyw)
			idx <- c(seq_len(n - 1L) + 1L, 1L)
			dx <- xyw[idx, 1L] - xyw[, 1L]
			dy <- xyw[idx, 2L] - xyw[, 2L]
			# Plain Coord2D (not Polygon2D) so normal2d() returns Coord2D
			edges <- Coord2D$new(cbind(x = dx, y = dy, w = rep.int(1, n)))
			norms <- normal2d(edges)
			private$normals_cache <- norms
			norms
		}
	)
)

#' Cast to Polygon2D object
#'
#' `as_polygon2d()` casts to a [Polygon2D] object.
#'
#' @param x Object to cast.  Either a [Coord2D] object of vertices or a
#'   numeric vector of x-coordinates.
#' @param ... Ignored; only included for S3 method consistency.
#' @return A [Polygon2D] object.
#' @examples
#' vertices <- as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
#' p <- as_polygon2d(vertices)
#' p$is_convex
#' print(p)
#' plot(p)
#' @export
as_polygon2d <- function(x, ...) {
	UseMethod("as_polygon2d")
}

#' @rdname as_polygon2d
#' @param convex `NA` to auto-detect (default), `TRUE` to assert convex,
#'   `FALSE` to mark as concave.
#' @export
as_polygon2d.Coord2D <- function(x, convex = NA, ...) {
	chkDots(...)
	Polygon2D$new(x$xyw, convex = convex)
}

#' @rdname as_polygon2d
#' @export
as_polygon2d.Polygon2D <- function(x, convex = NA, ...) {
	chkDots(...)
	if (is.na(convex)) {
		return(x)
	}
	Polygon2D$new(x$xyw, convex = convex)
}

#' @rdname as_polygon2d
#' @param y Numeric vector of y-coordinates (used when `x` is numeric).
#' @export
as_polygon2d.numeric <- function(x, y = 0, convex = NA, ...) {
	chkDots(...)
	as_polygon2d(as_coord2d(x = x, y = y), convex = convex)
}

#' @rdname as_polygon2d
#' @param n Number of vertices in the approximating polygon (default `60L`).
#' @param type `"inner"` (default) for an inscribed polygon whose vertices lie
#'   on the ellipse, or `"outer"` for a circumscribed polygon whose edges are
#'   tangent to the ellipse.  Both polygons are convex.
#' @export
as_polygon2d.Ellipse2D <- function(x, n = 60L, type = c("inner", "outer"), ...) {
	chkDots(...)
	stopifnot(length(x) == 1L)
	type <- match.arg(type)
	by <- 360 / n
	from <- switch(type, inner = 0, outer = -by / 2)
	t <- seq(from, by = by, length.out = n)
	scale <- if (type == "outer") 1 / cospi(1 / n) else 1
	rx <- x$rx * scale
	ry <- x$ry * scale
	ct <- cos(x$theta)
	st <- sin(x$theta)
	vx <- x$x + rx * cospi(t / 180) * ct - ry * sinpi(t / 180) * st
	vy <- x$y + rx * cospi(t / 180) * st + ry * sinpi(t / 180) * ct
	as_polygon2d(as_coord2d(vx, vy), convex = TRUE)
}

# Internal: check convexity of a polygon from its raw xyw matrix
is_convex_xyw <- function(xyw) {
	n <- nrow(xyw)
	if (n < 3L) {
		return(FALSE)
	}
	x <- xyw[, 1L]
	y <- xyw[, 2L]
	i2 <- c(seq_len(n - 1L) + 1L, 1L)
	i3 <- c(seq_len(n - 2L) + 2L, 1L, 2L)
	dx1 <- x[i2] - x
	dy1 <- y[i2] - y
	dx2 <- x[i3] - x[i2]
	dy2 <- y[i3] - y[i2]
	# z-component of the 3D cross product of those two edge vectors.
	# Its sign tells you which way you turned at vertex i+1.
	# - cross > 0: left turn (counterclockwise)
	# - cross < 0: right turn (clockwise)
	# - cross = 0: straight (collinear)
	cross <- dx1 * dy2 - dy1 * dx2
	# polygon is convex if and only if all turns go the same direction
	all(cross >= 0) || all(cross <= 0)
}
