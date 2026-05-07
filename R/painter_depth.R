#' Compute painter's depth
#'
#' `painter_depth()` computes a depth value for use with the painter's algorithm
#' (render lowest depth first).
#' For parallel (orthographic or oblique) projections, depth is a dot product
#' of the point with the projection direction vector derived from the plane and
#' oblique parameters.
#'
#' @param x Object to compute painter's depth for.
#' @param ... Passed to [as_plane3d()] or [as_angle()] as needed.
#' @param scale Oblique projection foreshortening scale factor.
#'   A value of `0` (default) indicates an orthographic projection.
#'   A value of `0.5` is used by a \dQuote{cabinet projection}
#'   while a value of `1.0` is used by a \dQuote{cavalier projection}.
#' @param alpha Oblique projection angle (the angle the off-axis direction is
#'   projected going off at).
#'   An [angle()] object or one coercible to one with `as_angle(alpha, ...)`.
#'   Popular angles are 45 degrees, 60 degrees, and `arctangent(2)` degrees.
#' @examples
#' # Coord2D: oblique projection with scale = 0.5, alpha = 45 degrees
#' p <- as_coord2d(x = c(1, 2, 3), y = c(1, 1, 2))
#' painter_depth(p, scale = 0.5, alpha = degrees(45))
#'
#' # Coord3D: orthographic projection onto xy-plane (depth = z)
#' p3 <- as_coord3d(x = 1:3, y = 1:3, z = c(1, 2, 3))
#' painter_depth(p3)
#'
#' # Segment2D: depth at midpoints
#' p1 <- as_coord2d(x = c(0, 2), y = c(0, 0))
#' p2 <- as_coord2d(x = c(2, 2), y = c(0, 2))
#' s <- as_segment2d(p1, p2 = p2)
#' painter_depth(s, scale = 0.5, alpha = degrees(45))
#'
#' # Polygon2D: depth of each edge (for ordering edge rendering)
#' vertices <- as_coord2d(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0))
#' poly <- as_polygon2d(vertices)
#' painter_depth(poly, scale = 0.5, alpha = degrees(45))
#' @return A numeric vector of depth values.
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
