#' Test whether an object has a Coord1D class
#'
#' `is_coord1d()` tests whether an object has a "Coord1D" class
#'
#' @param x An object
#' @return A logical value
#'
#' @examples
#' p <- as_coord1d(x = sample.int(10, 3))
#' is_coord1d(p)
#' @export
is_coord1d <- function(x) {
	inherits(x, "Coord1D")
}

#' Test whether an object has a Coord2D class
#'
#' `is_coord2d()` tests whether an object has a "Coord2D" class
#'
#' @param x An object
#' @return A logical value
#'
#' @examples
#' p <- as_coord2d(x = sample.int(10, 3), y = sample.int(10, 3))
#' is_coord2d(p)
#' @export
is_coord2d <- function(x) {
	inherits(x, "Coord2D")
}

#' Test whether an object has a Coord3D class
#'
#' `is_coord3d()` tests whether an object has a "Coord3D" class
#'
#' @param x An object
#' @return A logical value
#'
#' @examples
#' p <- as_coord3d(x = sample.int(10, 3),
#'                 y = sample.int(10, 3),
#'                 z = sample.int(10, 3))
#' is_coord3d(p)
#' @export
is_coord3d <- function(x) {
	inherits(x, "Coord3D")
}

#' Test whether an object has a Point1D class
#'
#' `is_point1d()` tests whether an object has a "Point1D" class
#'
#' @param x An object
#' @return A logical value
#'
#' @examples
#' p <- as_point1d(a = 1, b = 5)
#' is_point1d(p)
#' @export
is_point1d <- function(x) inherits(x, "Point1D")

#' Test whether an object has a Line2D class
#'
#' `is_line2d()` tests whether an object has a "Line2D" class
#'
#' @param x An object
#' @return A logical value
#'
#' @examples
#' l <- as_line2d(a = 1, b = 2, c = 3)
#' is_line2d(l)
#' @export
is_line2d <- function(x) inherits(x, "Line2D")

#' Test whether an object has a Plane3D class
#'
#' `is_plane3d()` tests whether an object has a "Plane3D" class
#'
#' @param x An object
#' @return A logical value
#'
#' @examples
#' p <- as_plane3d(a = 1, b = 2, c = 3, 4)
#' is_plane3d(p)
#' @export
is_plane3d <- function(x) inherits(x, "Plane3D")
