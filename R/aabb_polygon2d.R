#' Axis-aligned bounding box polygon
#'
#' `aabb_polygon2d()` creates an axis-aligned bounding box [Polygon2D] object
#' covering the range of `x`.
#'
#' @param x A 2D object with a [range()] method that returns a [Coord2D]
#'   of length two (e.g. [Coord2D], [Ellipse2D], [Segment2D]).
#' @param ... Passed to [range()].
#' @return A [Polygon2D] object with `$is_convex == TRUE`.
#' @seealso [rectangle_polygon2d()] for creating a rectangle by center and
#'   dimensions. [range()] for the bounding-range methods.
#' @examples
#' # Bounding box for a set of points
#' pts <- as_coord2d(
#'   x = runif(20, min = 1, max = 3),
#'   y = runif(20, min = 1, max = 5)
#' )
#' p <- aabb_polygon2d(pts)
#' p$is_convex
#' plot(p, border = "black", col = "cyan")
#' points(pts, pch = 16, cex = 1.5)
#'
#' # Bounding box for an ellipse
#' e <- as_ellipse2d(as_coord2d(1, 1), rx = 2, ry = 1, theta = degrees(30))
#' p2 <- aabb_polygon2d(e)
#' plot(p2, border = "black", col = "cyan")
#' lines(e, col = "black", lwd = 2)
#' @export
aabb_polygon2d <- function(x, ...) {
	r <- range(x, ...)
	min_x <- r$x[[1L]]
	max_x <- r$x[[2L]]
	min_y <- r$y[[1L]]
	max_y <- r$y[[2L]]
	vx <- c(min_x, max_x, max_x, min_x)
	vy <- c(min_y, min_y, max_y, max_y)
	as_polygon2d(as_coord2d(vx, vy), convex = TRUE)
}
