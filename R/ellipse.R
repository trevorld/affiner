#' 2D ellipse R6 Class
#'
#' `Ellipse2D` is an [R6::R6Class()] object representing one or more
#' two-dimensional ellipses.  It inherits from [Coord2D] so its center(s)
#' can be used with `$x` / `$y` / `$xyw` etc.  The semi-axes `rx`/`ry`
#' and orientation angle `theta` are updated automatically by each
#' transformation method.
#'
#' When `rx == ry` (within floating-point tolerance) the ellipse is a
#' circle, which can be tested with the `$is_circle` active binding.
#'
#' @examples
#' # An ellipse
#' e <- as_ellipse2d(as_coord2d(0, 0), rx = 2, ry = 1, theta = degrees(45))
#' print(e)
#' e$is_circle
#'
#' # A circle (special case)
#' c1 <- as_ellipse2d(as_coord2d(0.5, 0.5), rx = 0.5)
#' print(c1)
#' c1$is_circle
#' @export
Ellipse2D <- R6Class(
	"Ellipse2D",
	inherit = Coord2D,
	public = list(
		#' @param xyw A matrix with three columns for homogeneous center coordinates
		#'   (`"x"`, `"y"`, `"w"`).
		#' @param rx Numeric vector of x-axis semi-radii.
		#' @param ry Numeric vector of y-axis semi-radii.
		#' @param theta An [angle()] vector (or numeric, interpreted using
		#'   the default angular unit) of rotation angles.
		initialize = function(xyw, rx, ry, theta) {
			super$initialize(xyw)
			private$rx_ <- as.numeric(rx)
			private$ry_ <- as.numeric(ry)
			if (!is_angle(theta)) {
				theta <- as_angle(theta)
			}
			private$theta_ <- theta
		},
		#' @param n Number of ellipses to print.  If `NULL` print all.
		#' @param ... Passed to [format.default()].
		print = function(n = NULL, ...) {
			nlen <- nrow(private$mat_xyw)
			unit <- angular_unit(self$theta)
			cat("<Ellipse2D[", nlen, "] (theta in ", unit, ")>\n", sep = "")
			m <- cbind(
				x = self$x,
				y = self$y,
				rx = self$rx,
				ry = self$ry,
				theta = as.numeric(self$theta)
			)
			if (!is.null(n) && n < nlen) {
				m <- m[seq_len(n), , drop = FALSE]
			}
			print_mat(m, ...)
			invisible(self)
		},
		#' @param mat `r r2i_transform2d_mat`
		transform = function(mat = transform2d()) {
			if (!is_transform2d(mat)) {
				mat <- as_transform2d(mat)
			}
			# Flush any pending lazy center transform first so centers and
			# shape parameters stay in sync.
			private$apply_any_delayed_transformations()
			# Apply center transform eagerly (bypassing lazy queue).
			private$mat_xyw <- private$mat_xyw %*% mat
			colnames(private$mat_xyw) <- c("x", "y", "w")
			# Apply the linear 2x2 part of mat to each ellipse's shape.
			M <- mat[1:2, 1:2]
			n <- length(self$rx)
			new_rx <- numeric(n)
			new_ry <- numeric(n)
			new_theta <- numeric(n)
			for (i in seq_len(n)) {
				# shape matrix S = R(theta) %*% diag(rx, ry) maps unit circle to ellipse
				# M %*% S is new shape matrix that maps unit circle to ellipse
				sv <- svd(M %*% ellipse_shape_matrix(self$rx[i], self$ry[i], self$theta[i]))
				# 2x2 matrix factors as U %*% diag(d) %*% t(V)
				# U rotates the result into its final orientation in world coordinates
				# diag(d) stretches unit circle into an axis-alined ellipse with semi-axes d[1] >= d[2]
				# t(V) rotates/reflects but doesn't change ellipse shape
				new_rx[i] <- sv$d[1L]
				new_ry[i] <- sv$d[2L]
				# angle of the first column of U which is direction of the major axis
				new_theta[i] <- atan2(sv$u[2L, 1L], sv$u[1L, 1L])
			}
			private$rx_ <- new_rx
			private$ry_ <- new_ry
			private$theta_ <- radians(new_theta)
			invisible(self)
		}
	),
	active = list(
		#' @field rx Numeric vector of x-axis semi-radii (in the ellipse local frame).
		rx = function() private$rx_,
		#' @field ry Numeric vector of y-axis semi-radii (in the ellipse local frame).
		ry = function() private$ry_,
		#' @field theta An [angle()] vector of rotation angles of the ellipse
		#'   x-axis relative to the global x-axis.
		theta = function() private$theta_,
		#' @field is_circle Logical vector; `TRUE` for each ellipse where
		#'   `rx == ry` within floating-point tolerance.
		is_circle = function() {
			tol <- .Machine$double.eps^0.5
			r_max <- pmax(abs(self$rx), abs(self$ry))
			abs(self$rx - self$ry) <= tol * pmax(r_max, 1)
		}
	),
	private = list(
		rx_ = NULL,
		ry_ = NULL,
		theta_ = NULL
	)
)

# Internal: build the 2x2 shape matrix S = R(theta) %*% diag(rx, ry).
# Maps the unit circle to the (uncentered) ellipse.
# theta may be an angle() object or plain numeric radians.
ellipse_shape_matrix <- function(rx, ry, theta) {
	ct <- cos(theta)
	st <- sin(theta)
	matrix(c(ct * rx, st * rx, -st * ry, ct * ry), 2L, 2L)
}

#' Cast to Ellipse2D object
#'
#' `as_ellipse2d()` casts to an [Ellipse2D] object.
#'
#' @param x Object to cast.  Either a [Coord2D] object of center(s) or a
#'   numeric vector of x-coordinates of center(s).
#' @param ... Ignored; only included for S3 method consistency.
#' @return An [Ellipse2D] object.
#' @examples
#' # Ellipse from Coord2D center
#' e <- as_ellipse2d(as_coord2d(0, 0), rx = 2, ry = 1, theta = degrees(30))
#' plot(e)
#'
#' # Multiple ellipses
#' e2 <- as_ellipse2d(x = c(0, 1), y = c(0, 1), rx = c(1, 2), ry = c(0.5, 1))
#' plot(e2)
#'
#' # Multiple circles from x, y, r vectors
#' c2 <- as_ellipse2d(x = c(0, 1), y = c(0, 1), rx = c(0.3, 0.4))
#' plot(c2)
#' @export
as_ellipse2d <- function(x, ...) {
	UseMethod("as_ellipse2d")
}

#' @rdname as_ellipse2d
#' @param r Numeric vector of (circular) radii (default `0.5`).
#' @param rx Numeric vector of x-axis semi-radii (default `r`).
#' @param ry Numeric vector of y-axis semi-radii (default `r`).
#' @param theta An [angle()] vector (or numeric interpreted using the default
#'   angular unit) of rotation angles (default `angle(0)`).
#' @export
as_ellipse2d.Coord2D <- function(x, ..., r = 0.5, rx = r, ry = r, theta = angle(0)) {
	chkDots(...)
	if (!is_angle(theta)) {
		theta <- as_angle(theta)
	}
	n <- max(length(x), length(rx), length(ry), length(theta))
	xyw <- x$xyw
	if (nrow(xyw) < n) {
		xyw <- xyw[rep_len(seq_len(nrow(xyw)), n), , drop = FALSE]
	}
	Ellipse2D$new(
		xyw,
		rx = rep_len(as.numeric(rx), n),
		ry = rep_len(as.numeric(ry), n),
		theta = rep(theta, length.out = n)
	)
}

#' @rdname as_ellipse2d
#' @param y Numeric vector of y-coordinates of center(s) (used when `x` is
#'   numeric).
#' @export
as_ellipse2d.numeric <- function(x, y = 0, ..., r = 0.5, rx = r, ry = rx, theta = angle(0)) {
	chkDots(...)
	as_ellipse2d(as_coord2d(x = x, y = y), rx = rx, ry = ry, theta = theta)
}
