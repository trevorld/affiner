#' Isotoxal `2n`-gon and star polygon
#'
#' `isotoxal_2ngon_polygon2d()` creates an isotoxal `2n`-gon [Polygon2D]
#' object centered at `(x, y)`.
#' `star_polygon2d()` is an alias.
#'
#' Isotoxal `2n`-gon polygons have `2n` vertices alternating between `n`
#' outer vertices on a circle of radius `radius` and `n` inner vertices on
#' a concentric circle of radius `radial_scale * radius`.
#'
#' @param n Number of outer vertices.
#' @param x X-coordinate of the center (default `0`).
#' @param y Y-coordinate of the center (default `0`).
#' @param radius Outer circumradius (default `0.5`).
#' @param radial_scale Inner radius as a fraction of `radius`.  Defaults to
#'   `isotoxal_2ngon_inner_radius(n, alpha = alpha, beta_ext = beta_ext, d = d)`.
#'   Exactly one of `radial_scale`, `alpha`, `beta_ext`, or `d` must be
#'   supplied.
#' @param theta Angle of the first outer vertex (default `degrees(90)`).
#'   Will be coerced by [degrees()].
#' @param ... Ignored.
#' @param alpha Interior angle of an outer vertex.  Will be coerced by
#'   [degrees()].
#' @param beta_ext Exterior angle of an inner vertex.  Will be coerced by
#'   [degrees()].
#' @param d Density (winding number) of the star polygon `|n/d|`.
#' @return A [Polygon2D] object whose vertices are in counter-clockwise order.
#' @seealso [isotoxal_2ngon_inner_radius()] to compute the radial scale.
#'   [rectangle_polygon2d()] for rectangles.
#'   [regular_ngon_polygon2d()] for regular convex polygons.
#'   <https://en.wikipedia.org/wiki/Isotoxal_figure#Isotoxal_polygons> and
#'   <https://en.wikipedia.org/wiki/Star_polygon#Isotoxal_star_simple_polygons>
#'   for more information on isotoxal polygons.
#' @examples
#' # |5/2| star (the verda stelo)
#' p <- isotoxal_2ngon_polygon2d(5, d = 2)
#' p$is_convex
#' plot(p, col = "#008000", border = NA)
#'
#' # `star_polygon2d()` is an alias
#' p2 <- star_polygon2d(5, d = 2)
#' all.equal(p, p2)
#' @export
isotoxal_2ngon_polygon2d <- function(
	n,
	x = 0,
	y = 0,
	radius = 0.5,
	radial_scale = isotoxal_2ngon_inner_radius(n, alpha = alpha, beta_ext = beta_ext, d = d),
	theta = degrees(90),
	...,
	alpha = NULL,
	beta_ext = NULL,
	d = NULL
) {
	chkDots(...)
	t_outer <- degrees(theta) + degrees(seq(0, by = 360 / n, length.out = n))
	t_inner <- degrees(theta) + degrees(seq(180 / n, by = 360 / n, length.out = n))
	r_inner <- radius * radial_scale
	vx <- c(rbind(radius * cos(t_outer), r_inner * cos(t_inner)))
	vy <- c(rbind(radius * sin(t_outer), r_inner * sin(t_inner)))
	convex <- radial_scale >= cospi(1 / n)
	as_polygon2d(as_coord2d(x + vx, y + vy), convex = convex)
}

#' @rdname isotoxal_2ngon_polygon2d
#' @export
star_polygon2d <- isotoxal_2ngon_polygon2d
