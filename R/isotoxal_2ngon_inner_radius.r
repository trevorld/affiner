#' Isotoxal `2n`-gon inner radius
#'
#' `isotoxal_2ngon_inner_radius()` computes the inner radius
#' of an isotoxal `2n`-gon polygon.
#' `star_inner_radius()` is an alias.
#'
#' Isotoxal `2n`-gon polygons are polygons with:
#'
#' * `2n` vertices alternating between `n` "outer" vertices evenly spaced on one circle and `n` "inner" vertices evenly spaced on smaller circle with the same center.
#' * Each edge of the polygon is of the same length.
#' * The outer vertices all have the same interior angle `alpha` and the
#'   inner vertices all have the same interior angle `beta`.
#' * They are a generalization of (the outlines of) concave simple "star" polygons that also includes convex polygons with an even number of vertices.
#' @param n The number of outer vertices.
#' @param outer_radius The outer radius of the isotoxal `2n`-gon.
#' @param ... Ignored.
#' @param alpha The interior angle of an outer vertex.  Will be coerced by [degrees()].
#' @param beta_ext The exterior angle of an inner vertex.  Will be coerced by [degrees()].
#' @param d The density aka winding number of the regular star polygon (outline) in which case this star is represented by `|n/d|`.
#' @examples
#' # |8/3| star has outer vertex internal angle 45 degrees
#' # and inner vertex external angle 90 degrees
#' isotoxal_2ngon_inner_radius(8, d = 3)
#' isotoxal_2ngon_inner_radius(8, alpha = 45)
#' isotoxal_2ngon_inner_radius(8, beta_ext = 90)
#' @seealso <https://en.wikipedia.org/wiki/Isotoxal_figure#Isotoxal_polygons> and <https://en.wikipedia.org/wiki/Star_polygon#Isotoxal_star_simple_polygons> for more information on isotoxal polygons.
#' @return A numeric vector
#' @export
isotoxal_2ngon_inner_radius <- function(
	n,
	outer_radius = 1,
	...,
	alpha = NULL,
	beta_ext = NULL,
	d = NULL
) {
	stopifnot(is.null(alpha) + is.null(beta_ext) + is.null(d) == 2L)

	if (!is.null(alpha)) {
		alpha <- as.numeric(degrees(alpha), "degrees")
	}
	if (!is.null(beta_ext)) {
		beta_ext <- as.numeric(degrees(beta_ext), "degrees")
		alpha <- beta_ext_to_alpha(n, beta_ext)
	}
	if (!is.null(d)) {
		alpha <- 180 * (1 - 2 * d / n)
	}
	if (n == 2L) {
		return(outer_radius * isotoxal_2_scale(alpha))
	}
	stopifnot(alpha >= 0, alpha <= 180 * (1 - 2 / n))
	# we'll work with external degree
	beta_ext <- alpha_to_beta_ext(n, alpha)
	t <- 360 / n
	xy1 <- as_coord2d(x = 1, y = 0)
	xy2 <- as_coord2d(degrees(t), radius = 1)
	xyc <- mean(c(xy1, xy2))
	xyf <- as_coord2d(degrees(t / 2), radius = 1)
	dist_f <- distance2d(xyc, xyf)
	a2 <- distance2d(xy1, xyc)
	beta <- (180 - beta_ext) / 2
	b <- a2 * sin(degrees(beta)) / sin(degrees(beta_ext / 2))
	r <- 1 - b - dist_f
	stopifnot(r >= 0)
	outer_radius * r
}

#' @rdname isotoxal_2ngon_inner_radius
#' @export
star_inner_radius <- isotoxal_2ngon_inner_radius

isotoxal_2_scale <- function(alpha) {
	stopifnot(alpha >= 0, alpha <= 90)
	a1 <- alpha / 2
	a2 <- 180 - 90 - a1
	r <- sin(degrees(a1)) / sin(degrees(a2))
	stopifnot(r >= 0)
	r
}

beta_ext_to_alpha <- function(n, beta_ext) {
	total <- (2 * n - 2) * 180
	inverse <- 360 - beta_ext
	alpha <- (total - n * inverse) / n
	alpha %% 360
}

alpha_to_beta_ext <- function(n, alpha) {
	total <- (2 * n - 2) * 180
	inverse <- (total - n * alpha) / n
	beta_ext <- 360 - inverse
	beta_ext %% 360
}
