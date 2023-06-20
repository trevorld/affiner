#' @export
`[.Coord2D` <- function(x, i) {
    Coord2D$new(x$xyw[i, , drop = FALSE])
}

#' @export
`[.Coord3D` <- function(x, i) {
    Coord3D$new(x$xyzw[i, , drop = FALSE])
}

#' @export
as.complex.Coord2D <- function(x, ...) {
    complex(real = x$x, imaginary = x$y)
}

#' @export
as.data.frame.Coord2D <- function(x, ...) {
    as.data.frame(x$xyw)
}

#' @export
as.data.frame.Coord3D <- function(x, ...) {
    as.data.frame(x$xyzw)
}

#' @export
as.matrix.Coord2D <- function(x, ...) {
    x$xyw
}

#' @export
as.matrix.Coord3D <- function(x, ...) {
    x$xyzw
}

#' @export
c.Coord2D <- function(...) {
    l <- list(...)
    stopifnot(all(vapply(l, is_coord2d, logical(1))))
    m <- do.call(rbind, lapply(l, as.matrix))
    Coord2D$new(m)
}

#' @export
c.Coord3D <- function(...) {
    l <- list(...)
    stopifnot(all(vapply(l, is_coord3d, logical(1))))
    m <- do.call(rbind, lapply(l, as.matrix))
    Coord3D$new(m)
}

# (oblique) scalar projection onto a (unit) vector parameterized by its (polar) [angle()]

#' @export
as.double.Coord2D <- function(x, theta = angle(0), ..., op_scale = 0) {
    if (!is_angle(theta))
        theta <- as_angle(theta, ...)
    stopifnot(length(theta) == 1)
    x$clone()$rotate(-theta)$shear(xy_shear = op_scale)$x
}

#' @export
length.Coord2D <- function(x) {
    nrow(x$.__enclos_env__$private$mat_xyw)
}

#' @export
length.Coord3D <- function(x) {
    nrow(x$.__enclos_env__$private$mat_xyzw)
}

#' @export
rep.Coord2D <- function(x, ..., length.out = NA_integer_) {
    if (isTRUE(length(x) == length.out)) return(x)
    id <- rep(seq.int(length(x)), ..., length.out = length.out)
    Coord2D$new(x$xyw[id, , drop = FALSE])
}

#' @export
rep.Coord3D <- function(x, ..., length.out = NA_integer_) {
    if (isTRUE(length(x) == length.out)) return(x)
    id <- rep(seq.int(length(x)), ..., length.out = length.out)
    Coord3D$new(x$xyzw[id, , drop = FALSE])
}

#' Compute Euclidean norm
#'
#' `abs()` computes the Euclidean norm for [Coord2D] class objects and [Coord3D] class objects.
#' @param x A [Coord2D] class object or [Coord2D] class object.
#' @examples
#'   z <- complex(real = 1:4, imaginary = 1:4)
#'   p <- as_coord2d(z)
#'   abs(p) # Euclidean norm
#'   sqrt(p * p) # Less efficient way to calculate same numbers
#'
#'   # In {base} R `abs()` calculates Euclidean norm of complex numbers
#'   all.equal(abs(p), abs(z))
#'   all.equal(Mod(p), Mod(z))
#'
#'   p3 <- as_coord3d(x = 1:4, y = 1:4, z = 1:4)
#'   abs(p3)
#' @return A numeric vector
#' @name norm
#' @export
abs.Coord2D <- function(x) {
    sqrt(rowSums(x$xyw[, 1:2, drop = FALSE]^2))
}

#' @rdname norm
#' @export
abs.Coord3D <- function(x) {
    sqrt(rowSums(x$xyzw[, 1:3, drop = FALSE]^2))
}

#' Compute centroids of coordinates
#'
#' `mean()`computes centroids for for [Coord2D] and [Coord3D] class objects
#'
#' @param x A [Coord2D] object or [Coord3D] object
#' @param ... Passed to [base::mean()]
#' @return A [Coord2D] or [Coord3D] class object of length one
#' @examples
#'  p <- as_coord2d(x = 1:4, y = 1:4)
#'  print(mean(p))
#'  print(sum(p) / length(p)) # less efficient alternative
#'
#'  p <- as_coord3d(x = 1:4, y = 1:4, z = 1:4)
#'  print(mean(p))
#' @name centroid
#' @export
mean.Coord2D <- function(x, ...) {
    as_coord2d(mean(x$x, ...), mean(x$y, ...))
}

#' @rdname centroid
#' @export
mean.Coord3D <- function(x, ...) {
    as_coord3d(mean(x$x, ...), mean(x$y, ...), mean(x$z, ...))
}

#' Compute convex hull
#'
#' `convex_hull()` is a S3 generic for computing the convex hull of an object.
#' There is an implemented method supporting [Coord2D] class objects
#' using [grDevices::chull()] to compute the convex hull.
#'
#' @param x An object representing object to compute convex hull of such as a [Coord2D] class object.
#' @param ... Further arguments passed to or from other methods.
#' @return An object of same class as `x` representing just the subset of points on the convex hull.
#'         The method for [Coord2D] class objects returns these points in counter-clockwise order.
#' @examples
#' p <- as_coord2d(x = rnorm(25), y = rnorm(25))
#' print(convex_hull(p))
#'
#' # Equivalent to following caculation using `grDevices::chull()`
#' all.equal(convex_hull(p),
#'           p[rev(grDevices::chull(as.list(p)))])
#' @export
convex_hull <- function(x, ...) {
    UseMethod("convex_hull")
}

#' @rdname convex_hull
#' @export
convex_hull.Coord2D <- function(x, ...) {
    x[rev(grDevices::chull(as.list(x)))]
}

# Group "Summary"

#' @export
is.na.Coord2D <- function(x) is.na(x$x) | is.na(x$y)

#' @export
is.na.Coord3D <- function(x) is.na(x$x) | is.na(x$y) | is.na(x$z)

#' @export
sum.Coord2D <- function(..., na.rm = FALSE) {
    l <- list(...)
    if (na.rm)
        l <- lapply(l, function(p) p[!is.na(p)])
    xs <- sum(sapply(l, function(p) sum(p$x)))
    ys <- sum(sapply(l, function(p) sum(p$y)))
    as_coord2d(xs, ys)
}

#' @export
sum.Coord3D <- function(..., na.rm = FALSE) {
    l <- list(...)
    if (na.rm)
        l <- lapply(l, function(p) p[!is.na(p)])
    xs <- sum(sapply(l, function(p) sum(p$x)))
    ys <- sum(sapply(l, function(p) sum(p$y)))
    zs <- sum(sapply(l, function(p) sum(p$z)))
    as_coord3d(xs, ys, zs)
}

#' @export
`+.Coord2D` <- function(e1, e2) {
    if (missing(e2)) {
        e1
    } else {
        plus_coord2d(e1, e2)
    }
}

#' @export
`+.Coord3D` <- function(e1, e2) {
    if (missing(e2)) {
        e1
    } else {
        plus_coord3d(e1, e2)
    }
}

#' @export
`-.Coord2D` <- function(e1, e2) {
    if (missing(e2)) {
        e1$clone()$scale(-1)
    } else {
        minus_coord2d(e1, e2)
    }
}

#' @export
`-.Coord3D` <- function(e1, e2) {
    if (missing(e2)) {
        e1$clone()$scale(-1)
    } else {
        minus_coord3d(e1, e2)
    }
}

#' @export
`*.Coord2D` <- function(e1, e2) {
    if (is_coord2d(e1) && is_coord2d(e2)) {
        inner_coord2d(e1, e2)
    } else if (is_coord2d(e1) && is.numeric(e2)) {
        e1$clone()$scale(e2)
    } else if (is.numeric(e1) && is_coord2d(e2)) {
        e2$clone()$scale(e1)
    } else {
        stop(paste("Don't know how to multiply objects of class",
                   sQuote(class(e1)[1]),
                   "and class",
                   sQuote(class(e2)[1])))
    }
}

#' @export
`*.Coord3D` <- function(e1, e2) {
    if (is_coord3d(e1) && is_coord3d(e2)) {
        inner_coord3d(e1, e2)
    } else if (is_coord3d(e1) && is.numeric(e2)) {
        e1$clone()$scale(e2)
    } else if (is.numeric(e1) && is_coord3d(e2)) {
        e2$clone()$scale(e1)
    } else {
        stop(paste("Don't know how to multiply objects of class",
                   sQuote(class(e1)[1]),
                   "and class",
                   sQuote(class(e2)[1])))
    }
}

#' @export
`/.Coord2D` <- function(e1, e2) {
    if (is_coord2d(e1) && is.numeric(e2)) {
        e1$clone()$scale(1 / e2)
    } else {
        stop(paste("Don't know how to divide objects of class",
                   sQuote(class(e1)[1]),
                   "and class",
                   sQuote(class(e2)[1])))
    }
}

#' @export
`/.Coord3D` <- function(e1, e2) {
    if (is_coord3d(e1) && is.numeric(e2)) {
        e1$clone()$scale(1 / e2)
    } else {
        stop(paste("Don't know how to divide objects of class",
                   sQuote(class(e1)[1]),
                   "and class",
                   sQuote(class(e2)[1])))
    }
}

inner_coord2d <- function(p1, p2) {
    n <- max(length(p1), length(p2))
    p1 <- rep_len(p1, n)
    p2 <- rep_len(p2, n)
    rowSums(p1$xyw[, 1:2, drop = FALSE] * p2$xyw[, 1:2, drop = FALSE])
}

inner_coord3d <- function(p1, p2) {
    n <- max(length(p1), length(p2))
    p1 <- rep_len(p1, n)
    p2 <- rep_len(p2, n)
    rowSums(p1$xyzw[, 1:3, drop = FALSE] * p2$xyzw[, 1:3, drop = FALSE])
}

plus_coord2d <- function(p1, p2) {
    stopifnot(is_coord2d(p1), is_coord2d(p2))
    if (length(p1) == 1)
        p2$clone()$translate(p1)
    else
        p1$clone()$translate(p2)
}

plus_coord3d <- function(p1, p2) {
    stopifnot(is_coord3d(p1), is_coord3d(p2))
    if (length(p1) == 1)
        p2$clone()$translate(p1)
    else
        p1$clone()$translate(p2)
}

minus_coord2d <- function(p1, p2) {
    stopifnot(is_coord2d(p1), is_coord2d(p2))
    p1$clone()$translate(p2$clone()$scale(-1))
}

minus_coord3d <- function(p1, p2) {
    stopifnot(is_coord3d(p1), is_coord3d(p2))
    p1$clone()$translate(p2$clone()$scale(-1))
}

# Group "Complex"

#' @export
Re.Coord2D <- function(z) {
    z$x
}

#' @export
Im.Coord2D <- function(z) {
    z$y
}

#' @export
Mod.Coord2D <- function(z) {
    sqrt(rowSums(z$xyw[, 1:2, drop = FALSE]^2))
}

#' @export
Conj.Coord2D <- function(z) {
    complex(real = z$x, imaginary = -z$y)
}

#' @export
Arg.Coord2D <- function(z) {
    atan2(z$y, z$x)
}

#' Compute 3D vector cross product
#'
#' `cross_product()` computes the cross product of two [Coord3D] class vectors.
#' @param x A [Coord3D] class vector.
#' @param y A [Coord3D] class vector.
#' @return A [Coord3D] class vector
#' @examples
#' x <- as_coord3d(2, 3, 4)
#' y <- as_coord3d(5, 6, 7)
#' cross_product(x, y)
#' @export
cross_product <- function(x, y) {
    stopifnot(is_coord3d(x), is_coord3d(y))
    n <- max(length(x), length(y))
    x <- rep_len(x, n)
    y <- rep_len(y, n)
    m <- matrix(1, nrow = n, ncol = 4, dimnames = list(NULL, c("x", "y", "z", "w")))
    m[, 1] <- x$y * y$z - x$z * y$y
    m[, 2] <- x$z * y$x - x$x * y$z
    m[, 3] <- x$x * y$y - x$y * y$x
    Coord3D$new(m)
}
