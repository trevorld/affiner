#' Regular n-gon polygon
#'
#' `regular_ngon_polygon2d()` creates a regular n-gon [Polygon2D] object
#' centered at `(x, y)` with circumradius `radius`.
#'
#' @param n Number of vertices.
#' @param x X-coordinate of the center (default `0`).
#' @param y Y-coordinate of the center (default `0`).
#' @param radius Circumradius: distance from center to each vertex
#'   (default `0.5`).
#' @param theta Angle of the first vertex (default `degrees(90)`).
#'   Will be coerced by [degrees()].
#' @return A [Polygon2D] object with `$is_convex == TRUE`.
#' @seealso [isotoxal_2ngon_polygon2d()] for isotoxal star polygons.
#' @examples
#' # A regular hexagon
#' p <- regular_ngon_polygon2d(6)
#' p$is_convex
#' plot(p)
#' @export
regular_ngon_polygon2d <- function(n, x = 0, y = 0, radius = 0.5, theta = degrees(90)) {
	t <- degrees(theta) + degrees(seq(0, by = 360 / n, length.out = n))
	as_polygon2d(as_coord2d(x + radius * cos(t), y + radius * sin(t)), convex = TRUE)
}
