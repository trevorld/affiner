#' Plot coordinates, points, lines, and planes
#'
#' [plot()] plots [Coord1D] and [Coord2D] class objects while
#' [points()] draws [Coord1D] and [Coord2D] class objects
#' and [lines()] draws [Point1D] and [Line2D] class objects to an existing plot.
#' If the suggested [ggplot2][ggplot2::ggplot2] and [rgl][rgl::rgl] packages are available we also register [ggplot2::autolayer()] methods for [Coord1D], [Coord2D], [Point1D], and [Line2D] class objects and a [rgl::plot3d()] method for [Coord3D] class objects.
#'
#' @param x A supported object to plot.
#' @param ... Passed to the underlying plot method.
#' @return Used for its side effect of drawing to the graphics device.
#' @examples
#' c1 <- as_coord2d(x = 0, y = 1:10)
#' l <- as_line2d(a = 1, b = -1, c = 0) # y = x
#' c2 <- c1$clone()$reflect(l)
#' plot(c1, xlim = c(-1, 11), ylim = c(-1, 11),
#'      main = "2D reflection across a line")
#' lines(l)
#' points(c2, col = "red")
#'
#' c1 <- as_coord2d(x = 1:10, y = 1:10)
#' l <- as_line2d(a = -1, b = 0, c = 0) # x = 0
#' c2 <- c1$clone()$project(l)
#' if (require("ggplot2", quietly = TRUE,
#'             include.only = c("ggplot", "autolayer", "labs"))) {
#'   ggplot() +
#'       autolayer(c1) +
#'       autolayer(l) +
#'       autolayer(c2, color = "red") +
#'       labs(title = "2D projection onto a line")
#' }
#'
#' c1 <- as_coord1d(x = seq.int(-4, -1))
#' pt <- as_point1d(a = 1, b = 0) # x = 0
#' c2 <- c1$clone()$reflect(pt)
#' plot(c1, xlim = c(-5, 5), main = "1D reflection across a point")
#' lines(pt)
#' points(c2, col = "red")
#'
#' # 3D reflection across a plane
#' c1 <- as_coord3d(x = 1:10, y = 1:10, z = 1:10)
#' pl <- as_plane3d(a = 0, b = 0, c = -1, d = 2) # z = 2
#' c2 <- c1$clone()$reflect(pl)
#' if (require("rgl", quietly = TRUE,
#'             include.only = c("plot3d", "planes3d", "points3d"))) {
#'   plot3d(c1, size = 8)
#'   planes3d(as.data.frame(pl), d =  pl$d, color = "grey", alpha = 0.6)
#'   points3d(as.data.frame(c2), col = "red", size = 8)
#' }
#' @name graphics
#' @export
plot.Coord1D <- function(x, ...) {
	plot(data.frame(x = x$x, y = 0), ..., ylab = "", yaxt = "n")
}

#' @rdname graphics
#' @importFrom graphics points
#' @export
points.Coord1D <- function(x, ...) {
	points(data.frame(x = x$x, y = 0), ...)
}

#' @rdname graphics
#' @importFrom graphics lines
#' @export
lines.Point1D <- function(x, ...) {
	graphics::abline(v = -x$b / x$a, ...)
}

#' @rdname graphics
#' @export
plot.Coord2D <- function(x, ...) {
	plot(as.data.frame(x)[, 1:2], ...)
}

#' @rdname graphics
#' @importFrom graphics points
#' @export
points.Coord2D <- function(x, ...) {
	points(as.data.frame(x)[, 1:2], ...)
}

#' @rdname graphics
#' @importFrom graphics points
#' @export
lines.Line2D <- function(x, ...) {
	index_v <- which(x$b == 0)
	l <- list()
	if (length(index_v)) {
		graphics::abline(v = -x$c / x$a, ...)
		x <- x[-index_v]
	}
	if (length(x)) {
		slope <- -x$a / x$b
		intercept <- -x$c / x$b
		# a,b only allow single values
		for (i in seq_len(length(x))) {
			graphics::abline(a = intercept[i], b = slope[i], ...)
		}
	}
}

#' @exportS3Method rgl::plot3d
plot3d.Coord3D <- function(x, ...) {
	rgl::plot3d(as.data.frame(x), ...)
}
