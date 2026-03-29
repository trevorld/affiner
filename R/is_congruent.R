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
is_congruent.angle <- function(x, y, ..., mod_turns = TRUE, tolerance = sqrt(.Machine$double.eps)) {
	if (!is_angle(y)) {
		y <- as_angle(y, unit = angular_unit(x))
	}
	x <- as.numeric(x, "pi-radians")
	y <- as.numeric(y, "pi-radians")
	if (mod_turns) {
		n <- max(length(x), length(y))
		x <- rep_len(x, n) %% 2
		y <- rep_len(y, n) %% 2
		# Half-turn angles near two and zero (within tolerance) are "equivalent"
		id1 <- which(x < tolerance & y + tolerance > 2 + x)
		y[id1] <- y[id1] - 2
		id2 <- which(y < tolerance & x + tolerance > 2 + y)
		x[id2] <- x[id2] - 2
	}
	abs(x - y) < tolerance
}
