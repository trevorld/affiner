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
#' @field a Numeric vector that parameterizes the line via the equation `a * x + b * y = c = 0`.
#' @field b Numeric vector that parameterizes the line via the equation `a * x + b * y = c = 0`.
#' @field c Numeric vector that parameterizes the line via the equation `a * x + b * y = c = 0`.
#' @export
Line2D <- R6Class("Line2D",
   public = list(
       #' @param a Numeric vector that parameterizes the line via the equation `a * x + b * y = c = 0`.
       #' @param b Numeric vector that parameterizes the line via the equation `a * x + b * y = c = 0`.
       #' @param c Numeric vector that parameterizes the line via the equation `a * x + b * y = c = 0`.
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

#' @rdname as_line2d
#' @param theta Angle of the line represented by an [angle()] vector.
#' @param p1 Point on the line represented by a [Coord2D] class object.
#' @export
as_line2d.angle <- function(theta, p1, ...) {
    if (!is_coord2d(p1))
        p1 <- as_coord2d(p1, ...)
    # a * x + b * y + c = 0
    # cos(theta) * x + sin(theta) * y + c = 0
    a <- cos(theta)
    b <- sin(theta)
    c <- -a * p1$x + -b * p1$y
    Line2D$new(a, b, c)
}

#' @rdname as_line2d
#' @param p2 Another point on the line represented by a [Coord2D] class object.
#' @export
as_line2d.Coord2D <- function(p1, p2, ...) {
    if (!is_coord2d(p2))
        p2 <- as_coord2d(p2, ...)
    stopifnot(all(p1 != p2))
    theta <- as_angle(p2 - p1, unit = "radians")
    as_line2d.angle(theta, p1)
}
