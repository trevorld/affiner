#' Angle vector aware trigonometric functions
#'
#' `sine()`, `cosine()`, `tangent()`, `secant()`, `cosecant()`, and `cotangent()` are
#' [angle()] aware trigonometric functions that allow for a user chosen angular unit.
#'
#' @inheritParams angle
#' @return A numeric vector
#'
#' @examples
#' sine(pi, "radians")
#' cosine(180, "degrees")
#' tangent(0.5, "turns")
#'
#' a <- angle(0.5, "turns")
#' secant(a)
#' cosecant(a)
#' cotangent(a)
#' @name trigonometric-functions
NULL

#' @rdname trigonometric-functions
#' @export
sine <- function(x, unit = getOption("affiner_angular_unit", "degrees")) {
    if (!is_angle(x))
        x <- as_angle(x, unit = unit)
    sin.angle(x)
}

#' @rdname trigonometric-functions
#' @export
cosine <- function(x, unit = getOption("affiner_angular_unit", "degrees")) {
    if (!is_angle(x))
        x <- as_angle(x, unit = unit)
    cos.angle(x)
}

#' @rdname trigonometric-functions
#' @export
tangent <- function(x, unit = getOption("affiner_angular_unit", "degrees")) {
    if (!is_angle(x))
        x <- as_angle(x, unit = unit)
    tan.angle(x)
}

#' @rdname trigonometric-functions
#' @export
secant <- function(x, unit = getOption("affiner_angular_unit", "degrees")) {
    if (!is_angle(x))
        x <- as_angle(x, unit = unit)
    1 / sin.angle(x)
}

#' @rdname trigonometric-functions
#' @export
cosecant <- function(x, unit = getOption("affiner_angular_unit", "degrees")) {
    if (!is_angle(x))
        x <- as_angle(x, unit = unit)
    1 / cos.angle(x)
}

#' @rdname trigonometric-functions
#' @export
cotangent <- function(x, unit = getOption("affiner_angular_unit", "degrees")) {
    if (!is_angle(x))
        x <- as_angle(x, unit = unit)
    1 / tan.angle(x)
}

#' Angle vector aware inverse trigonometric functions
#'
#' `arcsine()`, `arccosine()`, `arctangent()`,
#' `arcsecant()`, `arccosecant()`, and `arccotangent()` are
#' inverse trigonometric functions that return [angle()] vectors
#' with a user chosen angular unit.
#'
#' @inheritParams angle
#' @param x A numeric vector
#' @return An [angle()] vector
#'
#' @examples
#' arccosine(-1, "degrees")
#' arcsine(0, "turns")
#' arctangent(0, "gradians")
#' arccosecant(-1, "degrees")
#' arcsecant(1, "degrees")
#' arccotangent(1, "half-turns")
#'
#' # `base::atan2(y, x)` computes the angle of the vector from origin to (x, y)
#' as_angle(as_coord2d(x = 1, y = 1), "degrees")
#' @name inverse-trigonometric-functions
NULL

#' @rdname inverse-trigonometric-functions
#' @param tolerance If `x` greater than 1 (or less than -1) but is within a `tolerance`
#'                  of 1 (or -1) then it will be treated as 1 (or -1)
#' @export
arcsine <- function(x, unit = getOption("affiner_angular_unit", "degrees"),
                    tolerance = sqrt(.Machine$double.eps)) {
    idp1 <- which(x > 1 && x < 1 + tolerance)
    x[idp1] <- 1
    idn1 <- which(x < -1 && x > -1 - tolerance)
    x[idn1] <- -1
    unit <- standardize_angular_unit(unit)
    angle(from_radians(asin(x), unit), unit)
}

#' @rdname inverse-trigonometric-functions
#' @export
arccosine <- function(x, unit = getOption("affiner_angular_unit", "degrees"),
                      tolerance = sqrt(.Machine$double.eps)) {
    idp1 <- which(x > 1 && x < 1 + tolerance)
    x[idp1] <- 1
    idn1 <- which(x < -1 && x > -1 - tolerance)
    x[idn1] <- -1
    unit <- standardize_angular_unit(unit)
    angle(from_radians(acos(x), unit), unit)
}

#' @rdname inverse-trigonometric-functions
#' @param y A numeric vector or `NULL`.
#'          If `NULL` (default) we compute the 1-argument arctangent
#'          else we compute the 2-argument arctangent.
#'          For positive coordinates `(x, y)` then `arctangent(x = y/x) == arctangent(x = x, y = y)`.
#' @export
arctangent <- function(x, unit = getOption("affiner_angular_unit", "degrees"),
                       y = NULL) {
    unit <- standardize_angular_unit(unit)
    if (is.null(y))
        angle(from_radians(atan(x), unit), unit)
    else
        angle(from_radians(atan2(y, x), unit), unit)
}

#' @rdname inverse-trigonometric-functions
#' @export
arcsecant <- function(x, unit = getOption("affiner_angular_unit", "degrees")) {
    arcsine(1 / x, unit)
}

#' @rdname inverse-trigonometric-functions
#' @export
arccosecant <- function(x, unit = getOption("affiner_angular_unit", "degrees")) {
    arccosine(1 / x, unit)
}

#' @rdname inverse-trigonometric-functions
#' @export
arccotangent <- function(x, unit = getOption("affiner_angular_unit", "degrees")) {
    arctangent(1 / x, unit)
}
