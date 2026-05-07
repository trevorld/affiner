utils::globalVariables(c("group", "x", "xend", "y", "yend"))

#' @exportS3Method ggplot2::autolayer
autolayer.Coord1D <- function(x, ...) {
	ggplot2::geom_point(ggplot2::aes(x = x, y = y), ..., data = data.frame(x = x$x, y = 0))
}

#' @exportS3Method ggplot2::autolayer
autolayer.Coord2D <- function(x, ...) {
	ggplot2::geom_point(ggplot2::aes(x = x, y = y), ..., data = as.data.frame(x))
}

#' @exportS3Method ggplot2::autolayer
autolayer.Point1D <- function(x, ...) {
	ggplot2::geom_vline(xintercept = -x$b / x$a, ...)
}

#' @exportS3Method ggplot2::autolayer
autolayer.Polygon2D <- function(x, ...) {
	ggplot2::geom_polygon(ggplot2::aes(x = x, y = y), ..., data = data.frame(x = x$x, y = x$y))
}

#' @exportS3Method ggplot2::autolayer
autolayer.Ellipse2D <- function(x, n = 60L, ...) {
	polys <- lapply(seq_len(length(x)), function(i) {
		p <- as_polygon2d(x[i], n = n)
		data.frame(x = p$x, y = p$y, group = i)
	})
	df <- do.call(rbind, polys)
	ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, group = group), ..., data = df)
}

#' @exportS3Method ggplot2::autolayer
autolayer.Segment2D <- function(x, ...) {
	df <- data.frame(x = x$p1$x, y = x$p1$y, xend = x$p2$x, yend = x$p2$y)
	ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend), ..., data = df)
}

#' @exportS3Method ggplot2::autolayer
autolayer.Line2D <- function(x, ...) {
	index_v <- which(x$b == 0)
	l <- list()
	if (length(index_v)) {
		l[[1L]] <- ggplot2::geom_vline(xintercept = -x$c / x$a, ...)
		x <- x[-index_v]
	}
	if (length(x)) {
		slope <- -x$a / x$b
		intercept <- -x$c / x$b
		l[[2L]] <- ggplot2::geom_abline(slope = slope, intercept = intercept, ...)
	}
	l
}
