#' 1D coordinate vector R6 Class
#'
#' `Coord1D` is an [R6::R6Class()] object representing two-dimensional points
#' represented by Cartesian Coordinates.
#'
#' @examples
#' p <- as_coord1d(x = rnorm(100, 2))
#' print(p, n = 10L)
#' pc <- mean(p) # Centroid
#' # method chained affine transformation matrices are auto-pre-multiplied
#' p$
#'   translate(-pc)$
#'   reflect("origin")$
#'   print(n = 10L)
#' @export
Coord1D <- R6Class(
	"Coord1D",
	public = list(
		#' @param xw A matrix with three columns representing (homogeneous) coordinates.
		#'           The first column represents x coordinates and
		#'           the last column is all ones.
		#'           Column names should be "x" and "w".
		initialize = function(xw) {
			private$mat_xw <- xw
			private$mat_trans <- NULL
		},
		#' @param n Number of coordinates to print. If `NULL` print all of them.
		#' @param ... Passed to [format.default()].
		print = function(n = NULL, ...) {
			if (is.null(n) || n > nrow(private$mat_xw)) {
				n <- nrow(private$mat_xw)
			}
			cat("<Coord1D[", nrow(private$mat_xw), "]>\n", sep = "")
			if (n > 0) {
				print_mat(self$xw[1:n, , drop = FALSE], ...)
			}
			invisible(self)
		},
		#' @param point `r r2i_transform1d_point`
		#' @param ... Passed to [project1d()].
		project = function(point = as_point1d("origin"), ...) {
			self$transform(project1d(point, ...))
		},
		#' @param point `r r2i_transform1d_point`
		#' @param ... Passed to [reflect1d()].
		reflect = function(point = as_point1d("origin"), ...) {
			self$transform(reflect1d(point, ...))
		},
		#' @param x_scale `r r2i_transform_x_scale`
		scale = function(x_scale = 1) {
			if (length(x_scale) == 1L) {
				self$transform(scale1d(x_scale))
			} else {
				private$apply_any_delayed_transformations()
				private$mat_xw[, 1L] <- x_scale * private$mat_xw[, 1L]
				invisible(self)
			}
		},
		#' @param x `r r2i_transform1d_x`
		#' @param ... Passed to `as_coord1d(x, ...)` if `x` is not a [Coord1D] object
		translate = function(x = as_coord1d(0), ...) {
			if (!is_coord1d(x)) {
				x <- as_coord1d(x, ...)
			}
			if (length(x) == 1L) {
				self$transform(translate1d(x))
			} else {
				private$apply_any_delayed_transformations()
				private$mat_xw[, 1L] <- private$mat_xw[, 1L] + x$x
				invisible(self)
			}
		},
		#' @param mat `r r2i_transform1d_mat`
		transform = function(mat = transform1d()) {
			if (!is_transform1d(mat)) {
				mat <- as_transform1d(mat)
			}
			if (is.null(private$mat_trans)) {
				private$mat_trans <- mat
			} else {
				private$mat_trans <- private$mat_trans %*% mat
			}
			invisible(self)
		}
	),
	active = list(
		#' @field xw A two-column matrix representing the homogeneous coordinates.
		#'           The first column is the "x" coordinates
		#'           and the second column is all ones.
		xw = function() {
			private$apply_any_delayed_transformations()
			private$mat_xw
		},
		#' @field x A numeric vector of x-coordinates.
		x = function() {
			as.vector(self$xw[, 1L])
		}
	),
	private = list(
		mat_xw = NULL,
		mat_trans = NULL,
		apply_any_delayed_transformations = function() {
			if (!is.null(private$mat_trans)) {
				private$mat_xw <- private$mat_xw %*% private$mat_trans
				private$mat_trans <- NULL
				colnames(private$mat_xw) <- c("x", "w")
			}
		}
	)
)

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
#'   rotate(90, "degrees")$
#'   print(n = 10)
#' @export
Coord2D <- R6Class(
	"Coord2D",
	public = list(
		#' @param xyw A matrix with three columns representing (homogeneous) coordinates.
		#'             The first two columns represent x and y coordinates and
		#'             the last column is all ones.
		#'             Column names should be "x", "y", and "w".
		initialize = function(xyw) {
			private$mat_xyw <- xyw
			private$mat_trans <- NULL
		},
		#' @param permutation `r r2i_transform2d_permutation`
		permute = function(permutation = c("xy", "yx")) {
			self$transform(permute2d(permutation))
		},
		#' @param n Number of coordinates to print.  If `NULL` print all of them.
		#' @param ... Passed to [format.default()].
		print = function(n = NULL, ...) {
			if (is.null(n) || n > nrow(private$mat_xyw)) {
				n <- nrow(private$mat_xyw)
			}
			cat("<Coord2D[", nrow(private$mat_xyw), "]>\n", sep = "")
			if (n > 0) {
				print_mat(self$xyw[1:n, , drop = FALSE], ...)
			}
			invisible(self)
		},
		#' @param line `r r2i_transform2d_line`
		#' @param ... Passed to [project2d()]
		#' @param scale `r r2i_transform2d_scale`
		project = function(line = as_line2d("x-axis"), ..., scale = 0) {
			self$transform(project2d(line, ..., scale = scale))
		},
		#' @param line `r r2i_transform2d_line`
		#' @param ... Passed to [reflect2d()].
		reflect = function(line = as_line2d("x-axis"), ...) {
			self$transform(reflect2d(line, ...))
		},
		#' @param theta `r r2i_transform_theta`
		#' @param ... Passed to [as_angle()].
		rotate = function(theta = angle(0), ...) {
			if (!is_angle(theta)) {
				theta <- as_angle(theta, ...)
			}
			if (length(theta) == 1L) {
				self$transform(rotate2d(theta))
			} else {
				private$apply_any_delayed_transformations()
				x <- private$mat_xyw[, 1L]
				y <- private$mat_xyw[, 2L]
				private$mat_xyw[, 1L] <- x * cos(theta) - y * sin(theta)
				private$mat_xyw[, 2L] <- x * sin(theta) + y * cos(theta)
				invisible(self)
			}
		},
		#' @param x_scale `r r2i_transform_x_scale`
		#' @param y_scale `r r2i_transform_y_scale`
		scale = function(x_scale = 1, y_scale = x_scale) {
			if (length(x_scale) == 1L && length(y_scale) == 1L) {
				self$transform(scale2d(x_scale, y_scale))
			} else {
				private$apply_any_delayed_transformations()
				private$mat_xyw[, 1L] <- x_scale * private$mat_xyw[, 1L]
				private$mat_xyw[, 2L] <- y_scale * private$mat_xyw[, 2L]
				invisible(self)
			}
		},
		#' @param xy_shear `r r2i_transform2d_xy_shear`
		#' @param yx_shear `r r2i_transform2d_yx_shear`
		shear = function(xy_shear = 0, yx_shear = 0) {
			self$transform(shear2d(xy_shear, yx_shear))
		},
		#' @param x `r r2i_transform2d_x`
		#' @param ... Passed to `as_coord2d(x, ...)` if `x` is not a [Coord2D] object
		translate = function(x = as_coord2d(0, 0), ...) {
			if (!is_coord2d(x)) {
				x <- as_coord2d(x, ...)
			}
			if (length(x) == 1L) {
				self$transform(translate2d(x))
			} else {
				private$apply_any_delayed_transformations()
				private$mat_xyw[, 1L] <- private$mat_xyw[, 1L] + x$x
				private$mat_xyw[, 2L] <- private$mat_xyw[, 2L] + x$y
				invisible(self)
			}
		},
		#' @param mat `r r2i_transform2d_mat`
		transform = function(mat = transform2d()) {
			if (!is_transform2d(mat)) {
				mat <- as_transform2d(mat)
			}
			if (is.null(private$mat_trans)) {
				private$mat_trans <- mat
			} else {
				private$mat_trans <- private$mat_trans %*% mat
			}
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
			as.vector(self$xyw[, 1L])
		},
		#' @field y A numeric vector of y-coordinates.
		y = function() {
			as.vector(self$xyw[, 2L])
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
#' pc <- mean(p) # Centroid
#' # method chained affine transformation matrices are auto-pre-multiplied
#' p$
#'   translate(-pc)$
#'   reflect("xy-plane")$
#'   rotate("z-axis", degrees(90))$
#'   print(n = 10)
#' @export
Coord3D <- R6Class(
	"Coord3D",
	public = list(
		#' @param xyzw A matrix with four columns representing (homogeneous) coordinates.
		#'             The first three columns represent x, y, and z coordinates and
		#'             the last column is all ones.
		#'             Column names should be "x", "y", "z", and "w".
		initialize = function(xyzw) {
			private$mat_xyzw <- xyzw
			private$mat_trans <- NULL
		},
		#' @param permutation `r r2i_transform3d_permutation`
		permute = function(permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy")) {
			self$transform(permute3d(permutation))
		},
		#' @param n Number of coordinates to print.  If `NULL` print all of them.
		#' @param ... Passed to [format.default()].
		print = function(n = NULL, ...) {
			if (is.null(n) || n > nrow(private$mat_xyzw)) {
				n <- nrow(private$mat_xyzw)
			}
			cat("<Coord3D[", nrow(private$mat_xyzw), "]>\n", sep = "")
			if (n > 0) {
				print_mat(self$xyzw[1:n, , drop = FALSE], ...)
			}
			invisible(self)
		},
		#' @param plane `r r2i_transform3d_plane`
		#' @param ... Passed to [project3d()].
		#' @param scale `r r2i_transform3d_scale`
		#' @param alpha `r r2i_transform3d_alpha`
		project = function(
			plane = as_plane3d("xy-plane"),
			...,
			scale = 0,
			alpha = angle(45, "degrees")
		) {
			self$transform(project3d(plane, ..., scale = scale, alpha = alpha))
		},
		#' @param plane `r r2i_transform3d_plane`
		#' @param ... Passed to [reflect3d()].
		reflect = function(plane = as_plane3d("xy-plane"), ...) {
			self$transform(reflect3d(plane, ...))
		},
		#' @param axis `r r2i_transform3d_axis`
		#' @param theta `r r2i_transform_theta`
		#' @param ... Passed to [rotate3d()].
		rotate = function(axis = as_coord3d("z-axis"), theta = angle(0), ...) {
			self$transform(rotate3d(axis, theta, ...))
		},
		#' @param x_scale `r r2i_transform_x_scale`
		#' @param y_scale `r r2i_transform_y_scale`
		#' @param z_scale `r r2i_transform_z_scale`
		scale = function(x_scale = 1, y_scale = x_scale, z_scale = x_scale) {
			if (length(x_scale) == 1L && length(y_scale) == 1L && length(z_scale) == 1L) {
				self$transform(scale3d(x_scale, y_scale, z_scale))
			} else {
				private$apply_any_delayed_transformations()
				private$mat_xyzw[, 1L] <- x_scale * private$mat_xyzw[, 1L]
				private$mat_xyzw[, 2L] <- y_scale * private$mat_xyzw[, 2L]
				private$mat_xyzw[, 3L] <- z_scale * private$mat_xyzw[, 3L]
				invisible(self)
			}
		},
		#' @param xy_shear `r r2i_transform3d_xy_shear`
		#' @param xz_shear `r r2i_transform3d_xz_shear`
		#' @param yx_shear `r r2i_transform3d_yx_shear`
		#' @param yz_shear `r r2i_transform3d_yz_shear`
		#' @param zx_shear `r r2i_transform3d_zx_shear`
		#' @param zy_shear `r r2i_transform3d_zy_shear`
		shear = function(
			xy_shear = 0,
			xz_shear = 0,
			yx_shear = 0,
			yz_shear = 0,
			zx_shear = 0,
			zy_shear = 0
		) {
			self$transform(shear3d(xy_shear, xz_shear, yx_shear, yz_shear, zx_shear, zy_shear))
		},
		#' @param x `r r2i_transform3d_x`
		#' @param ... Passed to `as_coord3d(x, ...)` if `x` is not a [Coord3D] object
		translate = function(x = as_coord3d(0, 0, 0), ...) {
			if (!is_coord3d(x)) {
				x <- as_coord3d(x, ...)
			}
			if (length(x) == 1L) {
				self$transform(translate3d(x))
			} else {
				private$apply_any_delayed_transformations()
				private$mat_xyzw[, 1L] <- private$mat_xyzw[, 1L] + x$x
				private$mat_xyzw[, 2L] <- private$mat_xyzw[, 2L] + x$y
				private$mat_xyzw[, 3L] <- private$mat_xyzw[, 3L] + x$z
				invisible(self)
			}
		},
		#' @param mat `r r2i_transform3d_mat`
		transform = function(mat = transform3d()) {
			if (!is_transform3d(mat)) {
				mat <- as_transform3d(mat)
			}
			if (is.null(private$mat_trans)) {
				private$mat_trans <- mat
			} else {
				private$mat_trans <- private$mat_trans %*% mat
			}
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
			as.vector(self$xyzw[, 1L])
		},
		#' @field y A numeric vector of y-coordinates.
		y = function() {
			as.vector(self$xyzw[, 2L])
		},
		#' @field z A numeric vector of z-coordinates.
		z = function() {
			as.vector(self$xyzw[, 3L])
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
