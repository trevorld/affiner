#' Painter's depth, order, and sort methods
#'
#' `painter_depth()` computes depth values for use with the painter's algorithm.
#' For parallel (orthographic or oblique) projections, depth is a dot product
#' of the point with the projection direction vector derived from the plane and
#' oblique parameters.
#' `painter_order()` wraps [painter_depth()] and [order()] to return the integer
#' indices that sort objects farthest-first (the draw order for the painter's
#' algorithm).
#' `sort.Coord2D()` and `sort.Coord3D()` wrap
#' `painter_order()` to return the sorted object directly.
#' `sort.Coord2D()` also handles [Segment2D] objects via inheritance.
#'
#' @param x Object to compute painter's depth/order for, or to sort.
#' @param ... Passed to [as_plane3d()] or [as_angle()] as needed.
#' @param scale Oblique projection foreshortening scale factor.
#'   The 2D methods default to `1` since `0` yields zero depth for all
#'   `Coord2D` points (z is always 0), making depth-based ordering impossible.
#'   The `Coord3D` method defaults to `0` (orthographic projection, depth = z).
#'   A value of `0.5` is used by a \dQuote{cabinet projection}
#'   while a value of `1.0` is used by a \dQuote{cavalier projection}.
#'   All positive values produce the same ordering.
#' @param alpha Oblique projection angle (the angle the off-axis direction is
#'   projected going off at).
#'   An [angle()] object or one coercible to one with `as_angle(alpha, ...)`.
#'   Popular angles are 45 degrees, 60 degrees, and `arctangent(2)` degrees.
#' @param decreasing If `TRUE` sort/order closest first instead of farthest
#'   first.
#' @examples
#' # Coord2D: oblique projection with scale = 1, alpha = 45 degrees
#' p <- as_coord2d(x = c(1, 2, 3), y = c(3, 1, 2))
#' painter_depth(p)
#' painter_order(p)
#' sort(p)
#'
#' # Coord3D: orthographic projection onto xy-plane (depth = z)
#' p3 <- as_coord3d(x = 1:3, y = 1:3, z = c(3, 1, 2))
#' painter_depth(p3)
#' painter_order(p3)
#' sort(p3)
#'
#' # Segment2D: depth/order at midpoints
#' p1 <- as_coord2d(x = c(0, 2), y = c(0, 0))
#' p2 <- as_coord2d(x = c(2, 2), y = c(0, 2))
#' s <- as_segment2d(p1, p2 = p2)
#' painter_depth(s)
#' painter_order(s)
#' sort(s)
#'
#' # Polygon2D: depth/order of each edge
#' vertices <- as_coord2d(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0))
#' poly <- as_polygon2d(vertices)
#' painter_depth(poly)
#' painter_order(poly)
#' @rdname painter_depth
#' @export
painter_depth <- function(x, ...) {
	UseMethod("painter_depth")
}

#' @rdname painter_depth
#' @export
painter_depth.Coord2D <- function(x, ..., scale = 1, alpha = angle(45, "degrees")) {
	if (!is_angle(alpha)) {
		alpha <- as_angle(alpha, ...)
	}
	-scale * cos(alpha) * x$x - scale * sin(alpha) * x$y
}

#' @rdname painter_depth
#' @param permutation Either "xyz" (no permutation), "xzy" (permute y and z axes),
#'                    "yxz" (permute x and y axes), "yzx" (x becomes z, y becomes x, z becomes y),
#'                    "zxy" (x becomes y, y becomes z, z becomes x), "zyx" (permute x and z axes).
#'                    This permutation is applied before the projection.
#' @param plane A [Plane3D] class object representing the plane
#'         projected onto, or an object coercible to one using
#'         `as_plane3d(plane, ...)` such as "xy-plane", "xz-plane", or "yz-plane".
#' @param roll Rotation of the in-plane coordinate frame around the plane normal
#'             after the azimuth/inclination alignment.
#'             An [angle()] object or one coercible to one with `as_angle(roll, ...)`.
#'             Defaults to `angle(0)` (no roll), which preserves the azimuth/inclination convention.
#' @export
painter_depth.Coord3D <- function(
	x,
	permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy"),
	...,
	plane = as_plane3d("xy-plane"),
	scale = 0,
	alpha = angle(45, "degrees"),
	roll = angle(0, "degrees")
) {
	if (!is_plane3d(plane)) {
		plane <- as_plane3d(plane, ...)
	}
	if (!is_angle(alpha)) {
		alpha <- as_angle(alpha, ...)
	}
	if (!is_angle(roll)) {
		roll <- as_angle(roll, ...)
	}
	stopifnot(length(plane) == 1, length(alpha) == 1, length(roll) == 1)
	permutation <- match.arg(permutation)

	denom <- plane$a^2 + plane$b^2 + plane$c^2
	closest <- as_coord3d(
		-plane$a * plane$d / denom,
		-plane$b * plane$d / denom,
		-plane$c * plane$d / denom
	)
	azimuth <- as_angle(plane, type = "azimuth")
	inclination <- as_angle(plane, type = "inclination")
	z_axis <- Coord3D$new(matrix(
		c(0, 0, 1, 1),
		nrow = 1,
		dimnames = list(NULL, c("x", "y", "z", "w"))
	))
	y_axis <- Coord3D$new(matrix(
		c(0, 1, 0, 1),
		nrow = 1,
		dimnames = list(NULL, c("x", "y", "z", "w"))
	))
	p <- x$clone()$permute(permutation)$translate(-closest)$rotate(z_axis, -azimuth)$rotate(
		y_axis,
		-inclination
	)$rotate(z_axis, roll)
	p$z - scale * cos(alpha) * p$x - scale * sin(alpha) * p$y
}

#' @rdname painter_depth
#' @export
painter_depth.Segment2D <- function(x, ..., scale = 1, alpha = angle(45, "degrees")) {
	painter_depth(x$mid_point, ..., scale = scale, alpha = alpha)
}

#' @rdname painter_depth
#' @export
painter_depth.Polygon2D <- function(x, ..., scale = 1, alpha = angle(45, "degrees")) {
	painter_depth(x$edges, ..., scale = scale, alpha = alpha)
}

#' @rdname painter_depth
#' @return `painter_order()` returns an integer vector of indices.
#' @export
painter_order <- function(x, ...) {
	UseMethod("painter_order")
}

#' @rdname painter_depth
#' @export
painter_order.Coord2D <- function(
	x,
	...,
	decreasing = FALSE,
	scale = 1,
	alpha = angle(45, "degrees")
) {
	order(painter_depth(x, ..., scale = scale, alpha = alpha), decreasing = decreasing)
}

#' @rdname painter_depth
#' @export
painter_order.Coord3D <- function(
	x,
	permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy"),
	...,
	decreasing = FALSE,
	plane = as_plane3d("xy-plane"),
	scale = 0,
	alpha = angle(45, "degrees"),
	roll = angle(0, "degrees")
) {
	order(
		painter_depth(
			x,
			permutation = permutation,
			...,
			plane = plane,
			scale = scale,
			alpha = alpha,
			roll = roll
		),
		decreasing = decreasing
	)
}

#' @rdname painter_depth
#' @export
painter_order.Polygon2D <- function(
	x,
	...,
	decreasing = FALSE,
	scale = 1,
	alpha = angle(45, "degrees")
) {
	order(painter_depth(x, ..., scale = scale, alpha = alpha), decreasing = decreasing)
}

#' @rdname painter_depth
#' @return `sort.Coord2D()` and `sort.Coord3D()` return a sorted object of the
#'   same class as `x`.
#' @export
sort.Coord2D <- function(x, decreasing = FALSE, ..., scale = 1, alpha = angle(45, "degrees")) {
	x[painter_order(x, ..., decreasing = decreasing, scale = scale, alpha = alpha)]
}

#' @rdname painter_depth
#' @export
sort.Coord3D <- function(
	x,
	decreasing = FALSE,
	...,
	permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy"),
	plane = as_plane3d("xy-plane"),
	scale = 0,
	alpha = angle(45, "degrees"),
	roll = angle(0, "degrees")
) {
	x[painter_order(
		x,
		...,
		decreasing = decreasing,
		permutation = permutation,
		plane = plane,
		scale = scale,
		alpha = alpha,
		roll = roll
	)]
}
