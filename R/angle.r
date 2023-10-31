#' Angle vectors
#'
#' `angle()` creates angle vectors with user specified angular unit.
#` `degrees()`, `radians()`, and `pi_radians()` are convenience wrappers for
#'  those commonly used angular units.
#'
#' @param x An angle vector or an object to convert to it (such as a numeric vector)
#' @param unit A string of the desired angular unit.  Supports the following strings
#'            (note we ignore any punctuation and space characters as well as any trailing `s`'s
#'             e.g. "half turns" will be treated as equivalent to "halfturn"):
#'
#'   * "deg" or "degree"
#'   * "half-revolution", "half-turn", or "pi-radian"
#'   * "gon", "grad", "grade", or "gradian"
#'   * "rad" or "radian"
#'   * "rev", "revolution", "tr", or "turn"
#'
#' @return A numeric vector of class "angle".
#'         Its "unit" attribute is a standardized string of the specified angular unit.
#'
#' @seealso [as_angle()], [angular_unit()], and [angle-methods].
#'   <https://en.wikipedia.org/wiki/Angle#Units> for more information about angular units.
#' @examples
#'   # Different representations of the "same" angle
#'   angle(180, "degrees")
#'   angle(pi, "radians")
#'   angle(0.5, "turns")
#'   angle(200, "gradians")
#'   pi_radians(1)
#'
#'   a1 <- angle(180, "degrees")
#'   angular_unit(a1)
#'   is_angle(a1)
#'   as.numeric(a1, "radians")
#'   cos(a1)
#'
#'   a2 <- as_angle(a1, "radians")
#'   angular_unit(a2)
#'   is_congruent(a1, a2)
#' @export
angle <- function(x = numeric(),
                  unit = getOption("affiner_angular_unit", "degrees")) {
    as_angle(x, unit = unit)
}

new_angle <- function(x, unit) {
    class(x) <- c("angle", class(x))
    attr(x, "unit") <- unit
    x
}

#' @rdname angle
#' @export
degrees <- function(x) {
    as_angle(x, "degrees")
}

#' @rdname angle
#' @export
pi_radians <- function(x) {
    as_angle(x, "pi-radians")
}

#' @rdname angle
#' @export
radians <- function(x) {
    as_angle(x, "radians")
}

#' Test whether an object is an angle vector
#'
#' `is_angle()` tests whether an object is an angle vector
#'
#' @param x An object
#' @return A logical value
#' @examples
#' a <- angle(180, "degrees")
#' is_angle(a)
#' is_angle(pi)
#' @export
is_angle <- function(x) inherits(x, "angle")

standardize_angular_unit <- function(unit) {
    stopifnot(length(unit) == 1L)
    if (unit %in% c("degrees", "pi-radians", "gradians", "radians", "turns"))
        unit
    else
        switch(gsub("[[:punct:]]|[[:space:]]|s$", "", unit),
           degree = "degrees",
           deg = "degrees",

           halfturn = "pi-radians",
           halfrevolution = "pi-radians",
           piradian = "pi-radians",

           gon = "gradians",
           grad = "gradians",
           grade = "gradians",
           gradian = "gradians",

           radian = "radians",
           rad = "radians",

           rev = "turns",
           revolution = "turns",
           tr = "turns",
           turn = "turns",
           stop(paste("Do not recognize angular unit", sQuote(unit)))
           )
}

#' Get/set angular unit of angle vectors
#'
#' `angular_unit()` gets/sets the angular unit of [angle()] vectors.
#'
#' @param x An [angle()] vector
#' @param value A string of the desired angular unit.  See [angle()] for supported strings.
#' @return `angular_unit()` returns a string of `x`'s angular unit.
#' @examples
#'   a <- angle(seq(0, 360, by = 90), "degrees")
#'   angular_unit(a)
#'   print(a)
#'   angular_unit(a) <- "turns"
#'   angular_unit(a)
#'   print(a)
#' @export
angular_unit <- function(x) {
    stopifnot(is_angle(x))
    attr(x, "unit")
}

#' @rdname angular_unit
#' @export
`angular_unit<-` <- function(x, value) {
    stopifnot(is_angle(x))
    unit <- standardize_angular_unit(value)
    if (unit == attr(x, "unit")) {
        x
    } else {
        new_angle(as.numeric(x, unit = unit), unit)
    }
}

#' Test whether two objects are congruent
#'
#' `is_congruent()` is a S3 generic that tests whether two different objects are \dQuote{congruent}.
#' The `is_congruent()` method for [angle()] classes tests whether two angles are congruent.
#'
#' @param x,y Two objects to test whether they are \dQuote{"congruent"}.
#' @param ... Further arguments passed to or from other methods.
#' @return A logical vector
#'
#' @examples
#'   # Use `is_congruent()` to check if two angles are "congruent"
#'   a1 <- angle(180, "degrees")
#'   a2 <- angle(pi, "radians")
#'   a3 <- angle(-180, "degrees") # Only congruent modulus full turns
#'   a1 == a2
#'   isTRUE(all.equal(a1, a2))
#'   is_congruent(a1, a2)
#'   is_congruent(a1, a2, mod_turns = FALSE)
#'   a1 == a3
#'   isTRUE(all.equal(a1, a3))
#'   is_congruent(a1, a3)
#'   is_congruent(a1, a3, mod_turns = FALSE)
#'
#' @export
is_congruent <- function(x, y, ...) {
    UseMethod("is_congruent")
}

#' @rdname is_congruent
#' @export
is_congruent.numeric <- function(x, y, ..., tolerance = sqrt(.Machine$double.eps)) {
    abs(x - y) < tolerance
}

#' @rdname is_congruent
#' @param mod_turns If `TRUE` angles that are congruent modulo full turns will be considered \dQuote{congruent}.
#' @param tolerance Angles (coerced to half-turns) or numerics with differences smaller
#'                  than `tolerance` will be considered \dQuote{congruent}.
#' @export
is_congruent.angle <- function(x, y, ..., mod_turns = TRUE,
                               tolerance = sqrt(.Machine$double.eps)) {
    if (!is_angle(y))
        y <- as_angle(y, unit = angular_unit(x))
    x <- as.numeric(x, "pi-radians")
    y <- as.numeric(y, "pi-radians")
    if (mod_turns) {
        n <- max(length(x), length(y))
        x <- rep_len(x, n) %% 2
        y <- rep_len(y, n) %% 2
        # Half-turn angles near two and zero (within tolerance) are "equivalent"
        id1 <- which(x < tolerance && y + tolerance > 2 + x)
        y[id1] <- y[id1] - 2
        id2 <- which(y < tolerance && x + tolerance > 2 + y)
        x[id2] <- x[id2] - 2
    }
    abs(x - y) < tolerance
}

#' Implemented base methods for angle vectors
#'
#' We implemented methods for several base generics for the [angle()] vectors.
#'
#' * Mathematical [Ops] (in particular `+` and `-`)
#'   for two angle vectors will (if necessary)
#'   set the second vector's [angular_unit()] to match the first.
#' * [as.numeric()] takes a `unit` argument which can be used to convert angles into other angular units
#'    e.g. `angle(x, "degrees") |> as.numeric("radians")` to cast a numeric vector `x` from degrees to radians.
#' * [abs()] will calculate the angle modulo full turns.
#' * Use [is_congruent()] to test if two angles are congruent instead of `==` or `all.equal()`.
#' * Not all implemented methods are documented here and since [angle()] is a
#'   [numeric()] class many other S3 generics
#'   besides the explicitly implemented ones should also work with it.
#'
#' @return Typical values as usually returned by these base generics.
#' @examples
#'   # Two "congruent" angles
#'   a1 <- angle(180, "degrees")
#'   a2 <- angle(pi, "radians")
#'
#'   print(a1)
#'   print(a1, unit = "radians")
#'   print(a1, unit = "pi-radians")
#'
#'   cos(a1)
#'   sin(a1)
#'   tan(a1)
#'
#'   # mathematical operations will coerce second `angle()` object to
#'   # same `angular_unit()` as the first one
#'   a1 + a2
#'   a1 - a2
#'
#'   as.numeric(a1)
#'   as.numeric(a1, "radians")
#'   as.numeric(a1, "turns")
#'
#'   # Use `is_congruent()` to check if two angles are "congruent"
#'   a1 == a2
#'   isTRUE(all.equal(a1, a2))
#'   is_congruent(a1, a2)
#'   is_congruent(a1, a2, mod_turns = FALSE)
#'   a3 <- angle(-180, "degrees") # Only congruent modulus full turns
#'   a1 == a3
#'   isTRUE(all.equal(a1, a2))
#'   is_congruent(a1, a3)
#'   is_congruent(a1, a3, mod_turns = FALSE)
#'
#' @param x [angle()] vector
#' @param ... Passed to [print.default()]
#' @inheritParams angle
#' @name angle-methods
NULL

#' @export
c.angle <- function(...) {
    l_dots <- list(...)
    unit <- angular_unit(l_dots[[1]])
    l_angles <- lapply(l_dots, as.numeric, unit = unit)
    angle(unlist(l_angles), unit)
}

#' @export
rep.angle <- function(x, ..., length.out = NA_integer_) {
    if (isTRUE(length(x) == length.out)) return(x)
    unit <- angular_unit(x)
    angle(rep(as.numeric(x, unit), ..., length.out = length.out),
          unit)
}

#' @rdname angle-methods
#' @export
as.double.angle <- function(x, unit = angular_unit(x), ...) {
    if (missing(unit)) return(NextMethod())
    unit <- standardize_angular_unit(unit)
    unit0 <- angular_unit(x)
    a <- NextMethod()
    if (unit == unit0) {
        a
    } else if (unit == "radians") {
        to_radians(a, unit0)
    } else if (unit0 == "radians") {
        from_radians(a, unit)
    } else {
        from_piradians(to_piradians(a, unit0),
                       unit)
    }
}

#' @rdname angle-methods
#' @param modulus Numeric vector representing the complex numbers' modulus
#' @export
as.complex.angle <- function(x, modulus = 1, ...) {
    complex(modulus = modulus, argument = as.numeric(x, "radians"))
}

#' @rdname angle-methods
#' @param use_unicode If `TRUE` use Unicode symbols as appropriate.
#' @export
format.angle <- function(x, unit = angular_unit(x), ...,
                         use_unicode = l10n_info()[["UTF-8"]]) {
    if (!missing(unit))
        x <- as_angle(x, unit = standardize_angular_unit(unit))
    suffix <- switch(angular_unit(x),
                     degrees = ifelse(use_unicode, "\u00b0", " deg"),
                     "pi-radians" = ifelse(use_unicode, "\u03c0 rad", "*pi rad"),
                     gradians = " gon",
                     radians = " rad",
                     turns = " tr")
    if (length(x)) {
        s <- paste0(format(as.numeric(x), ...), suffix)
        is.na(s) <- is.na(x)
        s
    } else {
        character()
    }
}

one_turn <- function(unit) {
    switch(unit,
           degrees = 360,
           "pi-radians" = 2,
           gradians = 400,
           radians = 2 * pi,
           turns = 1)
}

#' @rdname angle-methods
#' @export
abs.angle <- function(x) {
    unit <- angular_unit(x)
    angle(as.numeric(x) %% one_turn(unit), unit)
}

#' @rdname angle-methods
#' @export
print.angle <- function(x, unit = angular_unit(x), ...,
                        use_unicode = l10n_info()[["UTF-8"]]) {
    if (length(x)) {
        print.default(format.angle(x, unit = unit, ...,
                                   use_unicode = use_unicode),
                      ..., quote = FALSE)
    } else {
        cat("angle(0)\n")
    }
}

#' @export
cos.angle <- function(x) {
    cospi(as.numeric(x, "pi-radians"))
}

#' @export
sin.angle <- function(x) {
    sinpi(as.numeric(x, "pi-radians"))
}

#' @export
tan.angle <- function(x) {
    tanpi(as.numeric(x, "pi-radians"))
}

#' @export
Arg.angle <- function(z) {
    as.numeric(z, "radians")
}

#' @export
Ops.angle <- function(e1, e2) {
    if (missing(e2)) {
        NextMethod()
    } else {
        if (is_angle(e1) && is_angle(e2)) {
            if (angular_unit(e1) != angular_unit(e2)) {
                angular_unit(e2) <- angular_unit(e1)
            }
            NextMethod()
        } else {
            NextMethod()
        }
    }
}

to_piradians <- function(x, unit = "pi-radians") {
    switch(unit,
           degrees = x / 180,
           "pi-radians" = x,
           gradians = x / 200,
           radians = x / pi,
           turns = 2 * x)
}

from_piradians <- function(x, unit = "pi-radians") {
    switch(unit,
           degrees = 180 * x,
           "pi-radians" = x,
           gradians = 200 * x,
           radians = pi * x,
           turns = 0.5 * x)
}

to_radians <- function(x, unit = "radians") {
    switch(unit,
           degrees = pi * x / 180,
           "pi-radians" = pi * x,
           gradians = pi * x / 200,
           radians = x,
           turns = 2 * pi * x
    )
}

from_radians <- function(x, unit = "radians") {
    switch(unit,
           degrees = 180 * x / pi,
           "pi-radians" = x / pi,
           gradians = 200 * x / pi,
           radians = x,
           turns = x / (2 * pi)
    )
}

#' Cast to angle vector
#'
#' `as_angle()` casts to an [angle()] vector
#'
#' @param x An R object to convert to a [angle()] vector
#' @param ... Further arguments passed to or from other methods
#' @inheritParams angle
#' @return An [angle()] vector
#' @examples
#' as_angle(angle(pi, "radians"), "pi-radians")
#' as_angle(complex(real = 0, imaginary = 1), "degrees")
#' as_angle(as_coord2d(x = 0, y = 1), "turns")
#' as_angle(200, "gradians")
#'
#' @export
as_angle <- function(x, unit = getOption("affiner_angular_unit", "degrees"), ...) {
    UseMethod("as_angle")
}

#' @rdname as_angle
#' @export
as_angle.angle <- function(x, unit = getOption("affiner_angular_unit", "degrees"), ...) {
    unit <- standardize_angular_unit(unit)
    if (unit == angular_unit(x)) {
        x
    } else {
        new_angle(as.numeric(x, unit), unit)
    }
}

#' @rdname as_angle
#' @export
as_angle.character <- function(x, unit = getOption("affiner_angular_unit", "degrees"),  ...) {
    unit <- standardize_angular_unit(unit)
    a <- vapply(x, as_angle_character_helper, double(1), USE.NAMES = FALSE, unit = unit)
    if (any(is.na(a) & !is.na(x)))
        warning("NAs introduced by coercion")
    new_angle(a, unit)
}

as_angle_character_helper <- function(x, unit) {
    switch(x,
           "x-axis" = from_piradians(0, unit),
           "y-axis" = from_piradians(0.5, unit),
           NA_real_)
}

#' @rdname as_angle
#' @export
as_angle.complex <- function(x, unit = getOption("affiner_angular_unit", "degrees"), ...) {
    unit <- standardize_angular_unit(unit)
    new_angle(from_radians(Arg(x), unit), unit)
}

#' @rdname as_angle
#' @export
as_angle.Coord2D <- function(x, unit = getOption("affiner_angular_unit", "degrees"), ...) {
    unit <- standardize_angular_unit(unit)
    radians <- atan2(x$y, x$x)
    new_angle(from_radians(radians, unit), unit)
}

#' @rdname as_angle
#' @param type Use "azimuth" to calculate the azimuthal angle and "inclination" to calculate the inclination angle aka polar angle.
#' @export
as_angle.Coord3D <- function(x, unit = getOption("affiner_angular_unit", "degrees"),
                             type = c("azimuth", "inclination"), ...) {
    unit <- standardize_angular_unit(unit)
    type <- match.arg(type)
    switch(type,
           azimuth = new_angle(from_radians(atan2(x$y, x$x), unit), unit),
           inclination = arccosine(x$z / abs(x), unit = unit)
           )
}

#' @rdname as_angle
#' @export
as_angle.Line2D <- function(x, unit = getOption("affiner_angular_unit", "degrees"), ...) {
    arctangent(x = x$a, y = x$b, unit = unit)
}

#' @rdname as_angle
#' @export
as_angle.Plane3D <- function(x, unit = getOption("affiner_angular_unit", "degrees"),
                             type = c("azimuth", "inclination"), ...) {
    as_angle.Coord3D(normal3d(x), unit = unit, type = type)
}

#' @rdname as_angle
#' @export
as_angle.numeric <- function(x, unit = getOption("affiner_angular_unit", "degrees"), ...) {
    unit <- standardize_angular_unit(unit)
    new_angle(x, unit)
}
