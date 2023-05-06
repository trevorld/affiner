#' Test if 2D affine transformation matrix
#'
#' `is_transform2d()` tests if object is a [transform2d()] affine transformation matrix
#'
#' @param x An object
#' @return A logical value
#' @examples
#' m <- transform2d(diag(3))
#' is_transform2d(m)
#' is_transform2d(diag(3))
#' @export
is_transform2d <- function(x) {
    inherits(x, "transform2d")
}

#' Test if 3D affine transformation matrix
#'
#' `is_transform3d()` tests if object is a [transform3d()] affine transformation matrix
#'
#' @param x An object
#' @return A logical value
#' @examples
#' m <- transform3d(diag(4))
#' is_transform3d(m)
#' is_transform3d(diag(4))
#' @export
is_transform3d <- function(x) {
    inherits(x, "transform3d")
}

#' Cast to 2D affine transformation matrix
#'
#' `as_transform2d()` casts to a [transform2d()] affine transformation matrix
#'
#' @param x An object that can be cast to a
#' @param ... Further arguments passed to or from other methods
#' @return A [transform2d()] object
#' @examples
#' m <- diag(3)
#' as_transform2d(m)
#' @export
as_transform2d <- function(x, ...) {
    UseMethod("as_transform2d")
}

#' Cast to 3D affine transformation matrix
#'
#' `as_transform3d()` casts to a [transform3d()] affine transformation matrix
#'
#' @param x An object that can be cast to a
#' @param ... Further arguments passed to or from other methods
#' @return A [transform3d()] object
#' @examples
#' m <- diag(4)
#' as_transform3d(m)
#' @export
as_transform3d <- function(x, ...) {
    UseMethod("as_transform3d")
}

#' @rdname as_transform2d
#' @export
as_transform2d.transform2d <- function(x, ...) {
    x
}

#' @rdname as_transform3d
#' @export
as_transform3d.transform3d <- function(x, ...) {
    x
}

#' @rdname as_transform2d
#' @export
as_transform2d.default <- function(x, ...) {
    transform2d(as.matrix(x, ...))
}

#' @rdname as_transform3d
#' @export
as_transform3d.default <- function(x, ...) {
    transform3d(as.matrix(x, ...))
}

#' @export
t.at_matrix <- function(x) {
    class(x) <- NULL
    NextMethod()
}

is_at_matrix <- function(x) inherits(x, "at_matrix")

#' @export
Ops.at_matrix <- function(e1, e2) {
    if (is_at_matrix(e1))
        class(e1) <- NULL
    if (!missing(e2) && is_at_matrix(e2))
        class(e2) <- NULL
    NextMethod()
}

#' @export
Complex.at_matrix <- function(z) {
    class(z) <- NULL
    NextMethod()
}

#' @export
Math.at_matrix <- function(x, ...) {
    class(x) <- NULL
    NextMethod()
}

#' @export
solve.at_matrix <- function(a, b, ...) {
    m <- NextMethod()
    if (missing(b) && !is_at_matrix(m)) {
        class(m) <- class(a)
    }
    m
}

`%*%.at_matrix` <- function(x, y) {
    m <- NextMethod()
    if (is_at_matrix(x) && is_at_matrix(y) && !is_at_matrix(m)) {
        class(m) <- class(x)
    }
    m
}

#' @export
as.matrix.at_matrix <- function(x, ...) {
    class(x) <- NULL
    x
}
