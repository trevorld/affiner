#' 1D points R6 Class
#'
#' `Point1D` is an [R6::R6Class()] object representing one-dimensional points.
#'
#' @examples
#' p1 <- as_point1d(a = 1, b = 5)
#' @field a Numeric vector that parameterizes the point via the equation `a * x + b = 0`.
#' @field b Numeric vector that parameterizes the point via the equation `a * x + b = 0`.
#' @export
Point1D <- R6Class("Point1D",
   public = list(
       #' @param a Numeric vector that parameterizes the line via the equation `a * x + b = 0`.
       #' @param b Numeric vector that parameterizes the line via the equation `a * x + b = 0`.
       initialize = function(a, b) {
           stopifnot(length(a) == length(b))
           self$a <- a
           self$b <- b
       },
       #' @param n Number of lines to print.  If `NULL` print all of them.
       print = function(n = NULL) {
           if (is.null(n) || n > length(self$a))
               n <- length(self$a)
           cat("<Point1D[", length(self$a), "]>\n", sep = "")
           if (n > 0)
               print(cbind(a = self$a, b = self$b))
           invisible(self)
       },
       a = NULL, b = NULL)
)

#' 2D lines R6 Class
#'
#' `Line2D` is an [R6::R6Class()] object representing two-dimensional lines.
#'
#' @examples
#' p1 <- as_coord2d(x = 5, y = 10)
#' p2 <- as_coord2d(x = 7, y = 12)
#' theta <- degrees(45)
#' as_line2d(theta, p1)
#' as_line2d(p1, p2)
#' @field a Numeric vector that parameterizes the line via the equation `a * x + b * y + c = 0`.
#' @field b Numeric vector that parameterizes the line via the equation `a * x + b * y + c = 0`.
#' @field c Numeric vector that parameterizes the line via the equation `a * x + b * y + c = 0`.
#' @export
Line2D <- R6Class("Line2D",
   public = list(
       #' @param a Numeric vector that parameterizes the line via the equation `a * x + b * y + c = 0`.
       #' @param b Numeric vector that parameterizes the line via the equation `a * x + b * y + c = 0`.
       #' @param c Numeric vector that parameterizes the line via the equation `a * x + b * y + c = 0`.
       initialize = function(a, b, c) {
           stopifnot(length(a) == length(b) && length(b) == length(c))
           self$a <- a
           self$b <- b
           self$c <- c
       },
       #' @param n Number of lines to print.  If `NULL` print all of them.
       print = function(n = NULL) {
           if (is.null(n) || n > length(self$a))
               n <- length(self$a)
           cat("<Line2D[", length(self$a), "]>\n", sep = "")
           if (n > 0)
               print(cbind(a = self$a, b = self$b, c = self$c))
           invisible(self)
       },
       a = NULL, b = NULL, c = NULL)
)

#' 3D planes R6 Class
#'
#' `Plane3D` is an [R6::R6Class()] object representing three-dimensional planes.
#'
#' @field a Numeric vector that parameterizes the plane via the equation `a * x + b * y + c * z + d = 0`.
#' @field b Numeric vector that parameterizes the plane via the equation `a * x + b * y + c * z + d = 0`.
#' @field c Numeric vector that parameterizes the plane via the equation `a * x + b * y + c * z + d = 0`.
#' @field d Numeric vector that parameterizes the plane via the equation `a * x + b * y + c * z + d = 0`.
#' @export
Plane3D <- R6Class("Plane3D",
   public = list(
       #' @param a Numeric vector that parameterizes the plane via the equation `a * x + b * y + c * z + d = 0`.
       #' @param b Numeric vector that parameterizes the plane via the equation `a * x + b * y + c * z + d = 0`.
       #' @param c Numeric vector that parameterizes the plane via the equation `a * x + b * y + c * z + d = 0`.
       #' @param d Numeric vector that parameterizes the plane via the equation `a * x + b * y + c * z + d = 0`.
       initialize = function(a, b, c, d) {
           stopifnot(length(a) == length(b) && length(b) == length(c) && length(c) == length(d))
           self$a <- a
           self$b <- b
           self$c <- c
           self$d <- d
       },
       #' @param n Number of lines to print.  If `NULL` print all of them.
       print = function(n = NULL) {
           if (is.null(n) || n > length(self$a))
               n <- length(self$a)
           cat("<Plane3D[", length(self$a), "]>\n", sep = "")
           if (n > 0)
               print(cbind(a = self$a, b = self$b, c = self$c, d = self$d))
           invisible(self)
       },
       a = NULL, b = NULL, c = NULL, d = NULL)
)

#' Cast to Point1D object
#'
#' `as_point1d()` casts to a [Point1D] object.
#'
#' @examples
#' p1 <- as_point1d(a = 1, b = 0)
#' @param ... Passed to other function such as `as_coord2d()`.
#' @export
as_point1d <- function(...) {
    UseMethod("as_point1d")
}

#' Cast to Line2D object
#'
#' `as_line2d()` casts to a [Line2D] object.
#'
#' @examples
#' p1 <- as_coord2d(x = 5, y = 10)
#' p2 <- as_coord2d(x = 7, y = 12)
#' theta <- degrees(45)
#' as_line2d(theta, p1)
#' as_line2d(p1, p2)
#' @param ... Passed to other function such as `as_coord2d()`.
#' @export
as_line2d <- function(...) {
    UseMethod("as_line2d")
}

#' Cast to Plane3D object
#'
#' `as_plane3d()` casts to a [Plane3D] object.
#'
#' @param ... Passed to other function such as `as_coord2d()`.
#' @export
as_plane3d <- function(...) {
    UseMethod("as_plane3d")
}

#' @rdname as_point1d
#' @param a,b Numeric vectors that parameterize the point via the equation `a * x + b = 0`.
#'            Note this means that `x = -b / a`.
#' @export
as_point1d.numeric <- function(a, b, ...) {
    n <- max(length(a), length(b))
    a <- rep_len(a, n)
    b <- rep_len(b, n)
    Point1D$new(a, b)
}

#' @rdname as_line2d
#' @param a,b,c Numeric vectors that parameterize the line via the equation `a * x + b * y + c = 0`.
#'              Note if `y = m * x + b` then `m * x + 1 * y + -b = 0`.
#' @export
as_line2d.numeric <- function(a, b, c, ...) {
    n <- max(length(a), length(b), length(c))
    a <- rep_len(a, n)
    b <- rep_len(b, n)
    c <- rep_len(c, n)
    Line2D$new(a, b, c)
}

#' @rdname as_plane3d
#' @param a,b,c,d Numeric vectors that parameterize the plane via the equation `a * x + b * y + c * z + d = 0`.
#' @export
as_plane3d.numeric <- function(a, b, c, d, ...) {
    n <- max(length(a), length(b), length(c), length(d))
    a <- rep_len(a, n)
    b <- rep_len(b, n)
    c <- rep_len(c, n)
    d <- rep_len(d, n)
    Plane3D$new(a, b, c, d)
}

#' @rdname as_line2d
#' @param theta Angle of the line represented by an [angle()] vector.
#' @param p1 Point on the line represented by a [Coord2D] class object.
#' @export
as_line2d.angle <- function(theta, p1 = as_coord2d("origin"), ...) {
    if (!is_coord2d(p1))
        p1 <- as_coord2d(p1, ...)
    # a * x + b * y + c = 0
    # cos(theta) * x + sin(theta) * y + c = 0
    a <- cos(theta)
    b <- sin(theta)
    c <- -a * p1$x + -b * p1$y
    Line2D$new(a, b, c)
}

#' @rdname as_point1d
#' @param x A (character) vector to be cast to a [Point1D] object
#' @export
as_point1d.character <- function(x, ...) {
    a <- as_point1d_character_a(x)
    p <- as_point1d(a, 0)
    if (any(is.na(p) & !is.na(x)))
        warning("NAs introduced by coercion")
    p
}

as_point1d_character_a <- function(x) {
    switch(x,
           "origin" = 1,
           "zero" = 1,
           NA_real_)
}

#' @rdname as_line2d
#' @param x A (character) vector to be cast to a [Line2D] object
#' @export
as_line2d.character <- function(x, ...) {
    a <- as_line2d_character_a(x)
    b <- as_line2d_character_b(x)
    l <- as_line2d(a, b, 0)
    if (any(is.na(l) & !is.na(x)))
        warning("NAs introduced by coercion")
    l
}

as_line2d_character_a <- function(x) {
    switch(x,
           "x-axis" = 1,
           "y-axis" = 0,
           NA_real_)
}

as_line2d_character_b <- function(x) {
    switch(x,
           "x-axis" = 0,
           "y-axis" = 1,
           NA_real_)
}

#' @rdname as_plane3d
#' @param x A (character) vector to be cast to a [Plane3D] object
#' @export
as_plane3d.character <- function(x, ...) {
    a <- normal3d_character_x(x)
    b <- normal3d_character_y(x)
    c <- normal3d_character_z(x)
    p <- as_plane3d(a, b, c, 0)
    if (any(is.na(p) & !is.na(x)))
        warning("NAs introduced by coercion")
    p
}

#' @rdname as_point1d
#' @param normal [Coord1D] class object.
#' @export
as_point1d.Coord1D <- function(normal, ...) {
    as_point1d(a = 1, b = -normal$x)
}

#' @rdname as_line2d
#' @param normal Normal vector to the line represented by a [Coord2D] class object. `p2` should be missing.
#' @param p2 Another point on the line represented by a [Coord2D] class object.
#' @export
as_line2d.Coord2D <- function(normal, p1 = as_coord3d("origin"), p2, ...) {
    if (!is_coord2d(p1))
        p1 <- as_coord2d(p1, ...)
    if (!missing(normal)) {
        theta <- as_angle(normal2d(normal), unit = "radians")
        stopifnot(missing(p2))
        n <- max(length(p1), length(normal))
        p1 <- rep_len(p1, n)
        theta <- rep_len(theta, n)
        as_line2d.angle(theta, p1)
    } else {
        if (!is_coord2d(p2))
            p2 <- as_coord2d(p2, ...)
        stopifnot(all(p1 != p2))
        theta <- as_angle(p2 - p1, unit = "radians")
        as_line2d.angle(theta, p1)
    }
}

#' @rdname as_plane3d
#' @param p1 Point on the plane represented by a [Coord3D] class object.
#' @param p2,p3 Points on the plane represented by [Coord3D] class objects.  `normal` should be missing.
#' @param normal Normal vector to the plane represented by a [Coord3D] class object. `p2` and `p3` should be missing.
#' @export
as_plane3d.Coord3D <- function(normal, p1 = as_coord3d("origin"), p2, p3, ...) {
    if (!is_coord3d(p1))
        p1 <- as_coord3d(p1, ...)
    if (!missing(normal)) {
        stopifnot(missing(p2), missing(p3))
        n <- max(length(p1), length(normal))
        p1 <- rep_len(p1, n)
        normal <- rep_len(normal, n)
        Plane3D$new(normal$x, normal$y, normal$z, -(normal * p1))
    } else {
        if (!is_coord3d(p2))
            p2 <- as_coord3d(p2, ...)
        if (!is_coord3d(p3))
            p3 <- as_coord3d(p3, ...)
        v1 <- p2 - p1
        v2 <- p3 - p1
        normal <- normal3d(v1, v2)
        Plane3D$new(normal$x, normal$y, normal$z, -(normal * p1))
    }
}

#' @rdname as_point1d
#' @param point A [Point1D] object
#' @export
as_point1d.Point1D <- function(point, ...) {
    point
}

#' @rdname as_line2d
#' @param line A [Line2D] object
#' @export
as_line2d.Line2D <- function(line, ...) {
    line
}

#' @rdname as_line2d
#' @param point A [Point1D] object
#' @export
as_line2d.Point1D <- function(point, b = 0, ...) {
    as_line2d(point$a, b, point$b)
}

#' @rdname as_plane3d
#' @param plane A [Plane3D] object
#' @export
as_plane3d.Plane3D <- function(plane, ...) {
    plane
}

#' @rdname as_plane3d
#' @param point A [Point1D] object
#' @export
as_plane3d.Point1D <- function(point, b = 0, c = 0, ...) {
    as_plane3d(point$a, b, c, point$b)
}

#' @rdname as_plane3d
#' @param line A [Line2D] object
#' @export
as_plane3d.Line2D <- function(line, c = 0, ...) {
    as_plane3d(line$a, line$b, c, line$c)
}
