#' 2D coordinate vector R6 Class
#'
#' `Coord2D` is an [R6::R6Class()] object representing two-dimensional points
#' represented by Cartesian Coordinates.
#'
#' @examples
#' p <- as_coord2d(x = rnorm(100, 2), y = rnorm(100, 2))
#' print(p, n = 10)
#' pc <- mean(p) # Centroid
#' # method chained affine transformation matrices are auto-pre-multiplied
#' p$
#'   translate(-pc)$
#'   shear(x = 1, y = 0)$
#'   reflect("x-axis")$
#'   rotate(90, "degrees")
#'
#' @export
Coord2D <- R6Class("Coord2D",
   public = list(
       #' @param xyw A matrix with three columns representing (homogeneous) coordinates.
       #'             The first two columns represent x and y coordinates and
       #'             the last column is all ones.
       #'             Column names should be "x", "y", and "w".
       initialize = function(xyw) {
           private$mat_xyw <- xyw
           private$mat_trans <- NULL
       },
       #' @param permutation Either "xy" (no permutation) or "yx" (permute x and y axes)
       permute = function(permutation = c("xy", "yx")) {
           self$transform(permute2d(permutation))
       },
       #' @param n Number of coordinates to print.  If `NULL` print all of them.
       print = function(n = NULL) {
           if (is.null(n) || n > nrow(private$mat_xyw))
               n <- nrow(private$mat_xyw)
           cat("<Coord2D[", nrow(private$mat_xyw), "]>\n", sep = "")
           if (n > 0)
               print(self$xyw[1:n, ])
           invisible(self)
       },
       #' @param theta An [angle()] object of length one or an object coercible to one by `as_angle(theta, ...)`.
       #'              It represents the angle (from the horizontal axis)
       #'              of the line going through the origin you wish to project to
       #'              (e.g. an angle of 0 corresponds to the x-axis and
       #'              an angle of 90 degrees corresponds to the y-axis).
       #' @param ... Passed to [project2d()]
       #' @param scale Oblique projection scale factor.
       #'              A degenerate `0` value indicates an orthogonal projection.
       project = function(theta = angle(0), ..., scale = 0) {
           self$transform(project2d(theta, ..., scale = scale))
       },
       #' @param theta An [angle()] object of length one or an object coercible to one by `as_angle(theta, ...)`.
       #'              It represents the angle (from the horizontal axis)
       #'              of the line going through the origin you wish to reflect across
       #'              (e.g. an angle of 0 corresponds to the x-axis and
       #'              an angle of 90 degrees corresponds to the y-axis).
       #' @param ... Passed to [as_angle()].
       reflect = function(theta = angle(0), ...) {
           self$transform(reflect2d(theta, ...))
       },
       #' @param theta An [angle()] object of length one or an object coercible to one by `as_angle(theta, ...)`.
       #'              How much to rotate around the origin.
       #' @param ... Passed to [as_angle()].
       rotate = function(theta = angle(0), ...) {
           if (!is_angle(theta))
               theta <- as_angle(theta, ...)
           if (length(theta) == 1) {
               self$transform(rotate2d(theta))
           } else {
               private$apply_any_delayed_transformations()
               x <- private$mat_xyw[, 1]
               y <- private$mat_xyw[, 2]
               private$mat_xyw[, 1] <- x * cos(theta) - y * sin(theta)
               private$mat_xyw[, 2] <- x * sin(theta) + y * cos(theta)
               invisible(self)
           }
       },
       #' @param x_scale Scaling factor to apply to x coordinates
       #' @param y_scale Scaling factor to apply to y coordinates
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
       #' @param xy_shear Horizontal shear factor: `x = x + xy_shear * y`
       #' @param yx_shear Vertical shear factor: `y = yx_shear * x + y`
       shear = function(xy_shear = 0, yx_shear = 0) {
           self$transform(shear2d(xy_shear, yx_shear))
       },
       #' @param x A [Coord2D] object of length one or an object coercible to one by `as_coord2d(x, ...)`].
       #' @param ... Passed to `as_coord2d(x, ...)` if `x` is not a [Coord2D] object
       translate = function(x = as_coord2d(0, 0), ...) {
           if (!is_coord2d(x))
               x <- as_coord2d(x, ...)
           if (length(x) == 1) {
               self$transform(translate2d(x))
           } else {
               private$apply_any_delayed_transformations()
               private$mat_xyw[, 1] <- private$mat_xyw[, 1] + x$x
               private$mat_xyw[, 2] <- private$mat_xyw[, 2] + x$y
               invisible(self)
           }
       },
       #' @param mat A 3x3 matrix representing a post-multiplied affine transformation matrix.
       #'            The last **column** must be equal to `c(0, 0, 1)`.
       #'            If the last **row** is `c(0, 0, 1)` you may need to transpose it
       #'            to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.
       #'            If a 2x2 matrix (such as a 2x2 post-multiplied 2D rotation matrix)
       #'            we'll quietly add a final column/row equal to `c(0, 0, 1)`.
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
        #' @field xyw A three-column matrix representing the homogeneous coordinates.
        #'             The first two columns are "x" and "y" coordinates
        #'             and the third column is all ones.
        xyw = function() {
            private$apply_any_delayed_transformations()
            private$mat_xyw
        },
        #' @field x A numeric vector of x-coordinates.
        x = function() {
            as.vector(self$xyw[, 1])
        },
        #' @field y A numeric vector of y-coordinates.
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

#' 3D coordinate vector R6 Class
#'
#' `Coord3D` is an [R6::R6Class()] object representing three-dimensional points
#' represented by Cartesian Coordinates.
#'
#' @examples
#' p <- as_coord3d(x = rnorm(100, 2), y = rnorm(100, 2), z = rnorm(100, 2))
#' print(p, n = 10)
#' mean(p) # Centroid
#' @export
Coord3D <- R6Class("Coord3D",
   public = list(
       #' @param xyzw A matrix with four columns representing (homogeneous) coordinates.
       #'             The first three columns represent x, y, and z coordinates and
       #'             the last column is all ones.
       #'             Column names should be "x", "y", "z", and "w".
       initialize = function(xyzw) {
           private$mat_xyzw <- xyzw
           private$mat_trans <- NULL
       },
       #' @param permutation Either "xyz" (no permutation), "xzy" (permute y and z axes),
       #'                    "yxz" (permute x and y axes), "yzx" (x becomes z, y becomes x, z becomes y),
       #'                    "zxy" (x becomes y, y becomes z, z becomes x), "zyx" (permute x and z axes)
       permute = function(permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy")) {
           self$transform(permute3d(permutation))
       },
       #' @param n Number of coordinates to print.  If `NULL` print all of them.
       print = function(n = NULL) {
           if (is.null(n) || n > nrow(private$mat_xyzw))
               n <- nrow(private$mat_xyzw)
           cat("<Coord3D[", nrow(private$mat_xyzw), "]>\n", sep = "")
           if (n > 0)
               print(self$xyzw[1:n, ])
           invisible(self)
       },
       #' @param normal A [Coord3D] class object representing the vector normal of the plane
       #'         you wish to reflect across or project to or an object coercible to one using `normal3d(normal, ...)`
       #'         such as "xy-plane", "xz-plane", or "yz-plane".
       #'         We will also (if necessary) coerce it to a unit vector.
       #' @param ... Passed to [project3d()].
       #' @param scale Oblique projection foreshortening scale factor.
       #'   A (degenerate) `0` value indicates an orthographic projection.
       #'   A value of `0.5` is used by a \dQuote{cabinet projection}
       #'   while a value of `1.0` is used by a \dQuote{cavalier projection}.
       #' @param alpha Oblique projection angle (the angle the third axis is projected going off at).
       #'              An [angle()] object or one coercible to one with `as_angle(alpha, ...)`.
       #'              Popular angles are 45 degrees, 60 degrees, and `arctangent(2)` degrees.
       project = function(normal = normal3d("xy-plane"), ...,  scale = 0, alpha = angle(45, "degrees")) {
           self$transform(project3d(normal, ..., scale = scale, alpha = alpha))
       },
       #' @param normal A [Coord3D] class object representing the vector normal of the plane
       #'         you wish to reflect across or project to or an object coercible to one using `normal3d(normal, ...)`
       #'         such as "xy-plane", "xz-plane", or "yz-plane".
       #'         We will also (if necessary) coerce it to a unit vector.
       #' @param ... Passed to [reflect3d()].
       reflect = function(normal = normal3d("xy-plane"), ...) {
           self$transform(reflect3d(normal, ...))
       },
       #' @param axis A [Coord3D] class object or one that can coerced to one by `as_coord3d(axis, ...)`.
       #'             The `axis` represents the axis to be rotated around.
       #' @param theta An [angle()] object of length one or an object coercible to one by `as_angle(theta, ...)`.
       #' @param ... Passed to [rotate3d()].
       rotate = function(axis = as_coord3d("z-axis"), theta = angle(0), ...) {
           self$transform(rotate3d(axis, theta, ...))
       },
       #' @param x_scale Scaling factor to apply to x coordinates
       #' @param y_scale Scaling factor to apply to y coordinates
       #' @param z_scale Scaling factor to apply to z coordinates
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
       #' @param xy_shear Shear factor: `x = x + xy_shear * y + xz_shear * z`
       #' @param xz_shear Shear factor: `x = x + xy_shear * y + xz_shear * z`
       #' @param yx_shear Shear factor: `y = yx_shear * x + y + yz_shear * z`
       #' @param yz_shear Shear factor: `y = yx_shear * x + y + yz_shear * z`
       #' @param zx_shear Shear factor: `z = zx_shear * x + zy_shear * y + z`
       #' @param zy_shear Shear factor: `z = zx_shear * x + zy_shear * y + z`
       shear = function(xy_shear = 0, xz_shear = 0,
                        yx_shear = 0, yz_shear = 0,
                        zx_shear = 0, zy_shear = 0) {
           self$transform(shear3d(xy_shear, xz_shear, yx_shear, yz_shear, zx_shear, zy_shear))
       },
       #' @param x A [Coord3D] object of length one or an object coercible to one by `as_coord3d(x, ...)`]
       #' @param ... Passed to `as_coord3d(x, ...)` if `x` is not a [Coord3D] object
       translate = function(x = as_coord3d(0, 0, 0), ...) {
           if (!is_coord3d(x))
               x <- as_coord3d(x, ...)
           if (length(x) == 1) {
               self$transform(translate3d(x))
           } else {
               private$apply_any_delayed_transformations()
               private$mat_xyzw[, 1] <- private$mat_xyzw[, 1] + x$x
               private$mat_xyzw[, 2] <- private$mat_xyzw[, 2] + x$y
               private$mat_xyzw[, 3] <- private$mat_xyzw[, 3] + x$z
               invisible(self)
           }
       },
       #' @param mat A 4x4 matrix representing a post-multiplied affine transformation matrix.
       #'            The last **column** must be equal to `c(0, 0, 0, 1)`.
       #'            If the last **row** is `c(0, 0, 0, 1)` you may need to transpose it
       #'            to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.
       #'            If a 3x3 matrix (such as a 3x3 post-multiplied 3D rotation matrix)
       #'            we'll quietly add a final column/row equal to `c(0, 0, 0, 1)`.
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
        #' @field xyzw A four-column matrix representing the homogeneous coordinates.
        #'             The first three columns are "x", "y", and "z" coordinates
        #'             and the fourth column is all ones.
        xyzw = function() {
            private$apply_any_delayed_transformations()
            private$mat_xyzw
        },
        #' @field x A numeric vector of x-coordinates.
        x = function() {
            as.vector(self$xyzw[, 1])
        },
        #' @field y A numeric vector of y-coordinates.
        y = function() {
            as.vector(self$xyzw[, 2])
        },
        #' @field z A numeric vector of z-coordinates.
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
