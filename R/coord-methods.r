#' @export
`[.coord2d` <- function(x, i) {
    Coord2D$new(x$xyw[i, , drop = FALSE])
}

#' @export
`[.coord3d` <- function(x, i) {
    Coord3D$new(x$xyzw[i, , drop = FALSE])
}

#' @export
as.complex.coord2d <- function(x, ...) {
    complex(real = x$x, imaginary = x$y)
}

#' @export
as.data.frame.coord2d <- function(x, ...) {
    as.data.frame(x$xyw)
}

#' @export
as.data.frame.coord3d <- function(x, ...) {
    as.data.frame(x$xyzw)
}

#' @export
as.matrix.coord2d <- function(x, ...) {
    x$xyw
}

#' @export
as.matrix.coord3d <- function(x, ...) {
    x$xyzw
}

# "cross" product matrix
# https://en.wikipedia.org/wiki/Cross_product#Conversion_to_matrix_multiplication
cross_matrix <- function(x) {
    stopifnot(is_coord3d(x) && length(x) == 1)
    m <- matrix(0, nrow = 3, ncol = 3)
    m[1, 2] <- -x$z
    m[1, 3] <- x$y
    m[2, 1] <- x$z
    m[2, 3] <- -x$x
    m[3, 1] <- -x$y
    m[3, 2] <- x$x
    m
}

# (oblique) scalar projection onto a (unit) vector parameterized by its (polar) [angle()]

#' @export
as.double.coord2d <- function(x, theta = angle(0), ..., op_scale = 0) {
    if (!is_angle(theta))
        theta <- as_angle(theta, ...)
    stopifnot(length(theta) == 1)
    x$clone()$rotate(-theta)$shear(xy_shear = op_scale)$x
}

#' @export
length.coord2d <- function(x) {
    x$length
}

#' @export
length.coord3d <- function(x) {
    x$length
}

#' @export
rep.coord2d <- function(x, ..., length.out = NA_integer_) {
    if (isTRUE(length(x) == length.out)) return(x)
    id <- rep(seq.int(length(x)), ..., length.out = length.out)
    Coord2D$new(x$xyw[id, , drop = FALSE])
}

#' @export
rep.coord3d <- function(x, ..., length.out = NA_integer_) {
    if (isTRUE(length(x) == length.out)) return(x)
    id <- rep(seq.int(length(x)), ..., length.out = length.out)
    Coord3D$new(x$xyzw[id, , drop = FALSE])
}

#' Compute Euclidean norm
#'
#' `abs()` computes the Euclidean norm for [coord2d()] objects and [coord3d()] objects.
#' @param x A [coord2d()] object or [coord2d()] object.
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
#'   p3 <- coord3d(x = 1:4, y = 1:4, z = 1:4)
#'   abs(p3)
#' @return A numeric vector
#' @name norm
#' @export
abs.coord2d <- function(x) {
    sqrt(rowSums(x$xyw[, 1:2, drop = FALSE]^2))
}

#' @rdname norm
#' @export
abs.coord3d <- function(x) {
    sqrt(rowSums(x$xyzw[, 1:3, drop = FALSE]^2))
}

#' Compute centroids of coordinates
#'
#' `mean()`computes controids for for [coord2d()] and [coord3d()] objects
#'
#' @param x A [coord2d()] object or [coord3d()] object
#' @param ... Passed to [base::mean()]
#' @return A [coord2d()] or [coord3d()] object of length one
#' @examples
#'  p <- coord2d(x = 1:4, y = 1:4)
#'  mean(p)$print(usage = FALSE)
#'  (sum(p) / length(p))$print(usage = FALSE) # less efficient alternative
#'
#'  p <- coord3d(x = 1:4, y = 1:4, z = 1:4)
#'  mean(p)$print(usage = FALSE)
#' @name centroid
#' @export
mean.coord2d <- function(x, ...) {
    coord2d(mean(x$x, ...), mean(x$y, ...))
}

#' @rdname centroid
#' @export
mean.coord3d <- function(x, ...) {
    coord3d(mean(x$x, ...), mean(x$y, ...), mean(x$z, ...))
}

# Group "Summary"

#' @export
is.na.coord2d <- function(x) is.na(x$x) | is.na(x$y)

#' @export
is.na.coord3d <- function(x) is.na(x$x) | is.na(x$y) | is.na(x$z)

#' @export
sum.coord2d <- function(..., na.rm = FALSE) {
    l <- list(...)
    if (na.rm)
        l <- lapply(l, function(p) p[!is.na(p)])
    xs <- sum(sapply(l, function(p) sum(p$x)))
    ys <- sum(sapply(l, function(p) sum(p$y)))
    coord2d(xs, ys)
}

#' @export
sum.coord3d <- function(..., na.rm = FALSE) {
    l <- list(...)
    if (na.rm)
        l <- lapply(l, function(p) p[!is.na(p)])
    xs <- sum(sapply(l, function(p) sum(p$x)))
    ys <- sum(sapply(l, function(p) sum(p$y)))
    zs <- sum(sapply(l, function(p) sum(p$z)))
    coord3d(xs, ys, zs)
}

#' @export
`+.coord2d` <- function(e1, e2) {
    if (missing(e2)) {
        e1
    } else {
        plus_coord2d(e1, e2)
    }
}

#' @export
`+.coord3d` <- function(e1, e2) {
    if (missing(e2)) {
        e1
    } else {
        plus_coord3d(e1, e2)
    }
}

#' @export
`-.coord2d` <- function(e1, e2) {
    if (missing(e2)) {
        e1$clone()$scale(-1)
    } else {
        minus_coord2d(e1, e2)
    }
}

#' @export
`-.coord3d` <- function(e1, e2) {
    if (missing(e2)) {
        e1$clone()$scale(-1)
    } else {
        minus_coord3d(e1, e2)
    }
}

#' @export
`*.coord2d` <- function(e1, e2) {
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
`*.coord3d` <- function(e1, e2) {
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
`/.coord2d` <- function(e1, e2) {
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
`/.coord3d` <- function(e1, e2) {
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
    stopifnot(length(p1) == 1 || length(p2) == 1,
              is_coord2d(p1),
              is_coord2d(p2))
    if (length(p1) == 1)
        p2$clone()$translate(p1)
    else
        p1$clone()$translate(p2)
}

plus_coord3d <- function(p1, p2) {
    stopifnot(length(p1) == 1 || length(p2) == 1,
              is_coord3d(p1),
              is_coord3d(p2))
    if (length(p1) == 1)
        p2$clone()$translate(p1)
    else
        p1$clone()$translate(p2)
}

minus_coord2d <- function(p1, p2) {
    stopifnot(length(p2) == 1,
              is_coord2d(p1),
              is_coord2d(p2))
    p1$clone()$translate(p2$clone()$scale(-1))
}

minus_coord3d <- function(p1, p2) {
    stopifnot(length(p2) == 1,
              is_coord3d(p1),
              is_coord3d(p2))
    p1$clone()$translate(p2$clone()$scale(-1))
}

# Group "Complex"

#' @export
Re.coord2d <- function(z) {
    z$x
}

#' @export
Im.coord2d <- function(z) {
    z$y
}

#' @export
Mod.coord2d <- function(z) {
    sqrt(rowSums(z$xyw[, 1:2, drop = FALSE]^2))
}

#' @export
Conj.coord2d <- function(z) {
    complex(real = z$x, imaginary = -z$y)
}

#' @export
Arg.coord2d <- function(z) {
    atan2(z$y, z$x)
}
