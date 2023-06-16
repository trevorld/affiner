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
#'
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
#'
#' @export
is_coord3d <- function(x) {
    inherits(x, "Coord3D")
}
