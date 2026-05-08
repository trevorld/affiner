#' Rectangle polygon
#'
#' `rectangle_polygon2d()` creates a rectangle [Polygon2D] object
#' centered at `(x, y)`.
#'
#' @param width Width of the rectangle (default `1`).
#' @param height Height of the rectangle (default `1`).
#' @param x X-coordinate of the center (default `0`).
#' @param y Y-coordinate of the center (default `0`).
#' @param theta Rotation angle (default `degrees(0)`).
#'   Will be coerced by [degrees()].
#' @return A [Polygon2D] object with `$is_convex == TRUE`.
#' @seealso [isotoxal_2ngon_polygon2d()] for isotoxal star polygons.
#'   [regular_ngon_polygon2d()] for regular convex polygons.
#' @examples
#' # A unit square
#' p <- rectangle_polygon2d()
#' p$is_convex
#' plot(p)
#'
#' # A rotated rectangle
#' p2 <- rectangle_polygon2d(width = 2, height = 1, theta = degrees(45))
#' plot(p2)
#' @export
rectangle_polygon2d <- function(
	width = 1,
	height = 1,
	x = 0,
	y = 0,
	theta = degrees(0)
) {
	hw <- width / 2
	hh <- height / 2
	dx <- c(-hw, hw, hw, -hw)
	dy <- c(-hh, -hh, hh, hh)
	ct <- cos(degrees(theta))
	st <- sin(degrees(theta))
	vx <- x + dx * ct - dy * st
	vy <- y + dx * st + dy * ct
	as_polygon2d(as_coord2d(vx, vy), convex = TRUE)
}
