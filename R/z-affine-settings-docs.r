# The code for this function is in `standalone-affine-settings.r`

#' Compute `grid` affine transformation feature viewports and transformation functions
#'
#' `affine_settings()` computes `grid` group affine transformation feature viewport and transformation
#' function settings given the (x,y) coordinates of the corners of the
#' affine transformed "viewport" one wishes to draw in.
#'
#' @section Usage in other packages:
#'
#' To avoid taking a dependency on `affiner` you may copy the source of `affine_settings()`
#' into your own package under the permissive Unlicense.  Either use
#' `usethis::use_standalone("trevorld/affiner", "standalone-affine-settings.r")` or
#' copy the file `standalone-affine-settings.r` into your `R` directory and add `grid`
#' to the `Imports` of your `DESCRIPTION` file.
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
#'                    If `getRversion()` is less than `"4.2.0"` will instead be `NULL`.}
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
#' @seealso Intended for use with [affineGrob()] and [grid::useGrob()].
#'          See <https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html>
#'          for more information about the group affine transformation feature.
#' @export
affine_settings <- affine_settings
