#' @export
length.Line2D <- function(x) {
    length(x$a)
}

#' @export
rep.Line2D <- function(x, ..., length.out = NA_integer_) {
    if (isTRUE(length(x) == length.out)) return(x)
    id <- rep(seq.int(length(x)), ..., length.out = length.out)
    Line2D$new(x$a[id], x$b[id], x$c[id])
}

#' @export
c.Line2D <- function(...) {
    l <- list(...)
    stopifnot(all(vapply(l, is_line2d, logical(1))))
    a <- unlist(lapply(l, function(x) x$a))
    b <- unlist(lapply(l, function(x) x$b))
    c <- unlist(lapply(l, function(x) x$c))
    Line2D$new(a, b, c)
}

#' @export
is.na.Line2D <- function(x) is.na(x$a) | is.na(x$b) | is.na(x$c)

#' @export
is.nan.Line2D <- function(x) is.nan(x$a) | is.nan(x$b) | is.nan(x$c)

#' @export
is.finite.Line2D <- function(x) is.finite(x$a) & is.finite(x$b) & is.finite(x$c)

#' @export
is.infinite.Line2D <- function(x) is.infinite(x$a) | is.infinite(x$b) | is.infinite(x$c)

# "equality" is tricky unless we "normalize" the lines
# `==.Line2D`
# `!=.Line2D`
