#' 2D coordinate vectors
#'
#' `coord2d()` creates an [R6::R6Class()] object representing two-dimensional points
#' represented by Cartesian Coordinates.
#'
#' @param x A numeric vector of x coordinates
#' @param y A numeric vector of y coordinates
#' @examples
#' p <- coord2d(x = rnorm(100, 2), y = rnorm(100, 2))
#' print(p, usage = TRUE, n = 0)
#' print(p, usage = FALSE, n = 10)
#' pc <- mean(p) # Centroid
#' # method chained affine transformation matrices are auto-pre-multiplied
#' p$
#'   translate(-pc)$
#'   shear(x = 1, y = 0)$
#'   reflect("x-axis")$
#'   rotate(90, "degrees")
#'
#' @export
coord2d <- function(x = numeric(0), y = numeric(0)) {
    xyw <- cbind(x, y, rep_len(1, max(length(x), length(y))))
    Coord2D$new(as_xyw_matrix(xyw))
}

as_xyw_matrix <- function(x) {
    if (!is.matrix(x))
        x <- as.matrix(x)
    stopifnot(ncol(x) == 2 || ncol(x) == 3,
              is.numeric(x)
    )
    if (ncol(x) < 3)
        x <- cbind(x, 1)
    else
        stopifnot(all(x[, 3] == 1))
    colnames(x) <- c("x", "y", "w")
    x
}

#' 3D coordinate vectors
#'
#' `coord3d()` creates an [R6::R6Class()] object representing three-dimensional points
#' represented by Cartesian Coordinates.
#'
#' @param x A numeric vector of x coordinates
#' @param y A numeric vector of y coordinates
#' @param z A numeric vector of z coordinates
#' @examples
#' p <- coord3d(x = rnorm(100, 2), y = rnorm(100, 2), z = rnorm(100, 2))
#' print(p, usage = TRUE, n = 0)
#' print(p, usage = FALSE, n = 10)
#' pc <- mean(p) # Centroid
#'
#' @export
coord3d <- function(x = numeric(0), y = numeric(0), z = numeric(0)) {
    xyzw <- cbind(x, y, z, rep_len(1, max(length(x), length(y), length(z))))
    Coord3D$new(as_xyzw_matrix(xyzw))
}

as_xyzw_matrix <- function(x) {
    if (!is.matrix(x))
        x <- as.matrix(x)
    stopifnot(ncol(x) == 3 || ncol(x) == 4,
              is.numeric(x)
    )
    if (ncol(x) < 4)
        x <- cbind(x, 1)
    else
        stopifnot(all(x[, 4] == 1))
    colnames(x) <- c("x", "y", "z", "w")
    x
}

#' Test whether an object is a coord2d object
#'
#' `is_coord2d()` tests whether an object is a coord2d object
#'
#' @param x An object
#' @return A logical value
#'
#' @examples
#' p <- coord2d(x = sample.int(10, 3), y = sample.int(10, 3))
#' is_coord2d(p)
#'
#' @export
is_coord2d <- function(x) {
    inherits(x, "coord2d")
}

#' Test whether an object is a coord3d object
#'
#' `is_coord3d()` tests whether an object is a coord3d object
#'
#' @param x An object
#' @return A logical value
#'
#' @examples
#' p <- coord3d(x = sample.int(10, 3),
#'              y = sample.int(10, 3),
#'              z = sample.int(10, 3))
#' is_coord2d(p)
#'
#' @export
is_coord3d <- function(x) {
    inherits(x, "coord3d")
}

Coord2D <- R6Class("coord2d",
   public = list(
       initialize = function(xyw) {
           private$mat_xyw <- xyw
           private$mat_trans <- NULL
       },
       permute = function(permutation = c("xy", "yx")) {
           self$transform(permute2d(permutation))
       },
       print = function(n = NULL, usage = getOption("affiner_print_usage", FALSE)) {
           msg <- paste0("<", class(self)[1], "[", self$length, "]>")
           if (usage)
               msg <- c(msg,
                        "  Public Methods:",
                        "    clone()",
                        '    print(n = NULL, usage = getOption("affiner_print_usage", FALSE))',
                        "    project(theta = angle(0), ..., scale = 0)",
                        "    reflect(theta = angle(0), ...)",
                        "    rotate(theta = angle(0), ...)",
                        "    scale(x_scale = 1, y_scale = x_scale)",
                        "    shear(xy_shear = 0, yx_shear = 0)",
                        "    transform(mat = transform2d())",
                        "    translate(vec = coord2d(0, 0), ...)",
                        "  Active Bindings:",
                        "    x",
                        "    y",
                        "    xyw"
               )
           if (is.null(n) || n > self$length)
               n <- self$length
           if (n > 0) {
               if (usage) msg <- c(msg, "  Homogeneous Cartesian Coordinates:")
               cat(msg, sep = "\n")
               print(self$xyw[1:n, ])
           } else {
               cat(msg, sep = "\n")
           }
           invisible(self)
       },
       project = function(theta = angle(0), ..., scale = 0) {
           self$transform(project2d(theta, ..., scale = scale))
       },
       reflect = function(theta = angle(0), ...) {
           self$transform(reflect2d(theta, ...))
       },
       rotate = function(theta = angle(0), ...) {
           if (!is_angle(theta))
               theta <- as_angle(theta, ...)
           if (length(theta) == 1) {
               self$transform(rotate2d(theta, ...))
           } else {
               private$apply_any_delayed_transformations()
               x <- private$mat_xyw[, 1]
               y <- private$mat_xyw[, 2]
               private$mat_xyw[, 1] <- x * cos(theta) - y * sin(theta)
               private$mat_xyw[, 2] <- x * sin(theta) + y * cos(theta)
               invisible(self)
           }
       },
       scale = function(x_scale = 1, y_scale = x_scale) {
           if (length(x_scale) == 1 && length(y_scale) == 1) {
               self$transform(scale2d(x_scale, y_scale))
           } else {
               private$apply_any_delayed_transformations()
               private$mat_xyw[, 1] <- x_scale * private$mat_xyw[, 1]
               private$mat_xyw[, 2] <- y_scale * private$mat_xyw[, 2]
               invisible(self)
           }
       },
       shear = function(xy_shear = 0, yx_shear = 0) {
           self$transform(shear2d(xy_shear, yx_shear))
       },
       translate = function(vec = coord2d(0, 0), ...) {
           if (!is_coord2d(vec))
               vec <- as_coord2d(vec, ...)
           if (length(vec) == 1) {
               self$transform(translate2d(vec, ...))
           } else {
               private$apply_any_delayed_transformations()
               private$mat_xyw[, 1] <- private$mat_xyw[, 1] + vec$x
               private$mat_xyw[, 2] <- private$mat_xyw[, 2] + vec$y
               invisible(self)
           }
       },
       transform = function(mat = transform2d()) {
           if (!is_transform2d(mat))
               mat <- as_transform2d(mat)
           if (is.null(private$mat_trans))
               private$mat_trans <- mat
           else
               private$mat_trans <- private$mat_trans %*% mat
           invisible(self)
       }
    ),
    active = list(
        # avoid applying any delayed transformations just to know its length
        length = function() {
            nrow(private$mat_xyw)
        },
        xyw = function() {
            private$apply_any_delayed_transformations()
            private$mat_xyw
        },
        x = function() {
            as.vector(self$xyw[, 1])
        },
        y = function() {
            as.vector(self$xyw[, 2])
        }
    ),
    private = list(
        mat_xyw = NULL,
        mat_trans = NULL,
        apply_any_delayed_transformations = function() {
            if (!is.null(private$mat_trans)) {
                private$mat_xyw <- private$mat_xyw %*% private$mat_trans
                private$mat_trans <- NULL
                colnames(private$mat_xyw) <- c("x", "y", "w")
            }
        }
    )
)

Coord3D <- R6Class("coord3d",
   public = list(
       initialize = function(xyzw) {
           private$mat_xyzw <- xyzw
           private$mat_trans <- NULL
       },
       permute = function(permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy")) {
           self$transform(permute3d(permutation))
       },
       print = function(n = NULL, usage = getOption("affiner_print_usage", FALSE)) {
           msg <- paste0("<", class(self)[1], "[", self$length, "]>")
           if (usage)
               msg <- c(msg,
                        "  Public Methods:",
                        "    clone()",
                        '    permute(permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy"))',
                        '    print(n = NULL, usage = getOption("affiner_print_usage", FALSE))',
                        '    project(normal = as_coord3d("xy-plane"), ...,  scale = 0, alpha = angle(45, "degrees"))',
                        '    reflect(normal = as_coord3d("xy-plane"), ...)',
                        '    rotate(axis = as_coord3d("z-axis"), theta = angle(0), ...)',
                        "    scale(x_scale = 1, y_scale = x_scale, z_scale = x_scale)",
                        "    shear(xy_shear = 0, yx_shear = 0, yx_shear = 0, yz_shear = 0,",
                        "          zx_shear = 0, zy_shear = 0)",
                        "    transform(mat = transform3d())",
                        "    translate(vec = coord3d(0, 0, 0), ...)",
                        "  Active Bindings:",
                        "    x",
                        "    y",
                        "    z",
                        "    xyzw"
               )
           if (is.null(n) || n > self$length)
               n <- self$length
           if (n > 0) {
               if (usage) msg <- c(msg, "  Homogeneous Cartesian Coordinates:")
               cat(msg, sep = "\n")
               print(self$xyzw[1:n, ])
           } else {
               cat(msg, sep = "\n")
           }
           invisible(self)
       },
       project = function(normal = as_coord3d("xy-plane"), ...,  scale = 0, alpha = angle(45, "degrees")) {
           self$transform(project3d(normal, ..., scale = scale, alpha = alpha))
       },
       reflect = function(normal = as_coord3d("xy-plane"), ...) {
           self$transform(reflect3d(normal, ...))
       },
       rotate = function(axis = as_coord3d("z-axis"), theta = angle(0), ...) {
           self$transform(rotate3d(axis, theta, ...))
       },
       scale = function(x_scale = 1, y_scale = x_scale, z_scale = x_scale) {
           if (length(x_scale) == 1 && length(y_scale) == 1 && length(z_scale) == 1) {
               self$transform(scale3d(x_scale, y_scale, z_scale))
           } else {
               private$apply_any_delayed_transformations()
               private$mat_xyzw[, 1] <- x_scale * private$mat_xyzw[, 1]
               private$mat_xyzw[, 2] <- y_scale * private$mat_xyzw[, 2]
               private$mat_xyzw[, 3] <- z_scale * private$mat_xyzw[, 3]
               invisible(self)
           }
       },
       shear = function(xy_shear = 0, xz_shear = 0,
                        yx_shear = 0, yz_shear = 0,
                        zx_shear = 0, zy_shear = 0) {
           self$transform(shear3d(xy_shear, xz_shear, yx_shear, yz_shear, zx_shear, zy_shear))
       },
       translate = function(vec = coord3d(0, 0, 0), ...) {
           if (!is_coord3d(vec))
               vec <- as_coord3d(vec, ...)
           if (length(vec) == 1) {
               self$transform(translate3d(vec, ...))
           } else {
               private$apply_any_delayed_transformations()
               private$mat_xyzw[, 1] <- private$mat_xyzw[, 1] + vec$x
               private$mat_xyzw[, 2] <- private$mat_xyzw[, 2] + vec$y
               private$mat_xyzw[, 3] <- private$mat_xyzw[, 3] + vec$z
               invisible(self)
           }
       },
       transform = function(mat = transform3d()) {
           if (!is_transform3d(mat))
               mat <- as_transform3d(mat)
           if (is.null(private$mat_trans))
               private$mat_trans <- mat
           else
               private$mat_trans <- private$mat_trans %*% mat
           invisible(self)
       }
    ),
    active = list(
        # avoid applying any delayed transformations just to know its length
        length = function() {
            nrow(private$mat_xyzw)
        },
        xyzw = function() {
            private$apply_any_delayed_transformations()
            private$mat_xyzw
        },
        x = function() {
            as.vector(self$xyzw[, 1])
        },
        y = function() {
            as.vector(self$xyzw[, 2])
        },
        z = function() {
            as.vector(self$xyzw[, 3])
        }
    ),
    private = list(
        mat_xyzw = NULL,
        mat_trans = NULL,
        apply_any_delayed_transformations = function() {
            if (!is.null(private$mat_trans)) {
                private$mat_xyzw <- private$mat_xyzw %*% private$mat_trans
                private$mat_trans <- NULL
                colnames(private$mat_xyzw) <- c("x", "y", "z", "w")
            }
        }
    )
)
