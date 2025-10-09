utils::globalVariables(c("x", "y"))

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
	ggplot2::geom_vline(xintercept = -x$b / x$a)
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
