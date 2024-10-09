# ---
# repo: trevorld/affiner
# file: standalone-affine-settings.r
# last-updated: 2023-04-25
# license: https://unlicense.org
# imports: grid
# ---
#
# nocov start
#
# You may copy this source into your own R package
# by either using `usethis::use_standalone("trevorld/affiner", "standalone-affine-settings.r")`
# or simply copying this file into your `R` directory and adding `grid` to the `Imports` of
# your `DESCRIPTION` file.

#' Compute `grid` affine transformation feature viewports and transformation functions
#'
#' `affine_settings` computes `grid` group affine transformation feature viewport and transformation
#' function settings given the (x,y) coordinates of the corners of the
#' affine transformed "viewport" one wishes to draw in.
#'
#' @param xy An R object with named elements `x` and `y` representing the (x,y) coordinates
#'           of the affine transformed "viewport" one wishes to draw in.
#'           The (x,y) coordinates of the "viewport" should be in
#'           "upper left", "lower left", "lower right", and "upper right" order
#'           (this ordering should be from the perspective of **before**
#'           the "affine transformation" of the "viewport").
#' @param unit Which [grid::unit()] to assume the `xy` "x" and "y" coordinates are expressed in.
#' @return A named list with the following group affine transformation feature viewport
#'         and functions settings:\describe{
#'   \item{transform}{An affine transformation function to pass to [affineGrob()] or [useGrob()].
#'                    If `getRversion()` is less than `"4.2.0"` will instead return `NULL`.}
#'   \item{vp}{A [grid::viewport()] object to pass to [affineGrob()] or [useGrob()].}
#'   \item{sx}{x-axis sx factor}
#'   \item{flipX}{whether the affine transformed "viewport" is "flipped" horizontally}
#'   \item{x}{x-coordinate for viewport}
#'   \item{y}{y-coordinate for viewport}
#'   \item{width}{Width of viewport}
#'   \item{height}{Height of viewport}
#'   \item{default.units}{Default [grid::unit()] for viewport}
#'   \item{angle}{angle for viewport}
#' }
#' @seealso Intended for use with [affineGrob()] or perhaps [grid::useGrob()].
#'          See <https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html>
#'          for more information about the group affine transformation feature.
#' @keywords internal
#' @noRd
affine_settings <- function(xy = data.frame(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1)),
                            unit = getOption("affiner_grid_unit", "inches")) {
    df <- data.frame(x = xy$x, y = xy$y)
    stopifnot(nrow(df) == 4,
              unit %in% c("snpc",
                          "inches", "inch", "in",
                          "cm", "centimeter", "centimetre",
                          "mm", "points", "bigpts"))
    df <- at_label_face_coords(df)
    x <- at_x(df)
    y <- at_y(df)
    flipX <- at_is_flipped(df)
    origin <- at_origin(df, flipX)
    df <- at_translate_to_origin(df, origin)
    angle <- at_get_angle(df, flipX)
    df <- at_rotate(df, angle)
    width <- at_width(df)
    height <- at_height(df)
    sx <- at_shear_sx(df, flipX, height, width)

    vp <- grid::viewport(x = x, y = y, width = width, height = height,
                         default.units = unit, angle = angle)
    if (getRversion() >= "4.2.0") {
        transform <- function(group, ...) {
            grid::viewportTransform(group, ...,
                                    shear = grid::groupShear(sx = sx),
                                    flip = grid::groupFlip(flipX = flipX))
        }
    } else {
        transform <- NULL
    }

    list(transform = transform, vp = vp,
         sx = sx, flipX = flipX,
         x = x, y = y, width = width, height = height, default.units = unit,
         angle = angle)
}

# given (x,y) coordinates of four points in (affine transformed) rectangle
# label points before and after transformation by position in rectangle
at_label_face_coords <- function(df ) {
    df$before <- c("upper_left", "lower_left", "lower_right", "upper_right")
    df$after <- ""
    i_left <- order(df$x, -df$y) # leftmost points with tie-brakes by y

    # "lower-left" will be the lowest of the two left-most vertices
    if (df[i_left[1], "y"] < df[i_left[2], "y"]) {
        i_ll <- i_left[1]
    } else {
        i_ll <- i_left[2]
    }
    df[i_ll, "after"] <- "lower_left"
    df[shift_idx(i_ll, 2), "after"] <- "upper_right"
    # translate lower-left to origin and rotate so lower-left and upper-right on x-axis
    # then upper-left will be above x-axis and lower-right will be below x-axis
    df0 <- at_translate_to_origin(df, df[i_ll,])
    df0 <- at_rotate(df0, df0[shift_idx(i_ll, 2), "theta"])
    if (df0[shift_idx(i_ll, 1), "y"] > 0) {
        df[shift_idx(i_ll, 1), "after"] <- "upper_left"
        df[shift_idx(i_ll, 3), "after"] <- "lower_right"
    } else {
        df[shift_idx(i_ll, 1), "after"] <- "lower_right"
        df[shift_idx(i_ll, 3), "after"] <- "upper_left"
    }
    df
}

shift_idx <- function(x, i) {
    x2 <- (x + i) %% 4
    if (x2 == 0)
        4
    else
        x2
}

at_is_flipped <- function(df) {
    i_ul <- which(df$after == "upper_left")
    i_next <- ifelse(i_ul < 4L, i_ul + 1L, 1L)
    df[i_next, "after"] != "lower_left"
}

# translate "lower left" (after flipping but before rotation) to (0,0)
# add polar r and theta coordinates
at_translate_to_origin <- function(df, origin) {
    df$x <- df$x - origin$x
    df$y <- df$y - origin$y
    df$theta <- to_t(df$x, df$y)
    df$r <- to_r(df$x, df$y)
    df
}

at_shear_sx <- function(df, flipped, height, width) {
    if (flipped) {
        i_ll <- which(df$before == "lower_right")
        i_ul <- which(df$before == "upper_right")
    } else {
        i_ll <- which(df$before == "lower_left")
        i_ul <- which(df$before == "upper_left")
    }
    (df[i_ul, "x"] - df[i_ll, "x"]) / height
}

at_origin <- function(df, flipped = FALSE) {
    if (flipped)
        i_ll <- which(df$before == "lower_right")
    else
        i_ll <- which(df$before == "lower_left")

    df[i_ll, ]
}

at_x <- function(df) {
    mean(df$x)
}

at_y <- function(df) {
    mean(df$y)
}

at_get_angle <- function(df, flipped = FALSE) {
    if (flipped)
        i_lr <- which(df$before == "lower_left")
    else
        i_lr <- which(df$before == "lower_right")
    df[i_lr, "theta"]
}

at_rotate <- function(df, angle) {
    df$theta <- df$theta - angle
    df$x <- to_x(df$theta, df$r)
    df$y <- to_y(df$theta, df$r)
    df
}

at_width <- function(df) {
    x_ll <- df[which(df$before == "lower_left"), "x"]
    x_lr <- df[which(df$before == "lower_right"), "x"]
    abs(x_lr - x_ll)
}

at_height <- function(df) {
    y_ll <- df[which(df$before == "lower_left"), "y"]
    y_ul <- df[which(df$before == "upper_left"), "y"]
    abs(y_ul - y_ll)
}

# Convert between Cartesian and polar coordinates
to_x <- function(t, r) {
    r * cos(pi * t / 180)
}
to_y <- function(t, r) {
    r * sin(pi * t / 180)
}
to_r <- function(x, y) {
    sqrt(x^2 + y^2)
}
to_t <- function(x, y) {
    180 * atan2(y, x) / pi
}

# nocov end
