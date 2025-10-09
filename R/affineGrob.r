#' Affine transformation grob
#'
#' `affineGrob()` is a grid grob function to facilitate
#' using the group affine transformation features introduced in R 4.2.
#'
#' `r affine_transformation_support`
#' @param grob A grid grob to perform affine transformations on.  Passed to [grid::defineGrob()] as its `src` argument.
#' @param vp_define [grid::viewport()] to define grid group in.  Passed to [grid::defineGrob()] as its `vp` argument.
#'                  This will cumulative with the current viewport and the `vp` argument (if any),
#'                  if this cumulative viewport falls outside the graphics device drawing area this
#'                  grob may be clipped on certain graphics devices.
#' @param transform An affine transformation function.
#'                  If `NULL` default to [grid::viewportTransform()].
#'                  Passed to [grid::useGrob()] as its `transform` argument.
#' @param vp_use [grid::viewport()] passed to [grid::useGrob()] as its `vp` argument.
#' @param name A character identifier (for grid).
#' @param gp A [grid::gpar()] object.
#' @param vp A [grid::viewport()] object (or `NULL`).
#' @return A [grid::gTree()] (grob) object of class "affine".
#'         As a side effect `grid.affine()` draws to the active graphics device.
#' @examples
#' if (require("grid")) {
#'   grob <- grobTree(rectGrob(gp = gpar(fill = "blue", col = NA)),
#'                    circleGrob(gp=gpar(fill="yellow", col = NA)),
#'                    textGrob("RSTATS", gp=gpar(fontsize=32)))
#'   grid.newpage()
#'   pushViewport(viewport(width=unit(4, "in"), height=unit(2, "in")))
#'   grid.draw(grob)
#'   popViewport()
#' }
#'
#' if (require("grid") &&
#'     getRversion() >= "4.2.0" &&
#'     isTRUE(dev.capabilities()$transformations)) {
#'   # Only works if active graphics device supports affine transformations
#'   # such as `png(type="cairo")` on R 4.2+
#'   vp_define <- viewport(width=unit(2, "in"), height=unit(2, "in"))
#'   affine <- affineGrob(grob, vp_define=vp_define)
#'   grid.newpage()
#'   pushViewport(viewport(width=unit(4, "in"), height=unit(2, "in")))
#'   grid.draw(affine)
#'   popViewport()
#' }
#' if (require("grid") &&
#'     getRversion() >= "4.2.0" &&
#'     isTRUE(dev.capabilities()$transformations)) {
#'   # Only works if active graphics device supports affine transformations
#'   # such as `png(type="cairo")` on R 4.2+
#'   settings <- affine_settings(xy = list(x = c(3/3, 2/3, 0/3, 1/3),
#'                                         y = c(2/3, 1/3, 1/3, 2/3)),
#'                               unit = "snpc")
#'   affine <- affineGrob(grob,
#'                        vp_define = vp_define,
#'                        transform = settings$transform,
#'                        vp_use = settings$vp)
#'   grid.newpage()
#'   grid.draw(affine)
#' }
#' @seealso See [affine_settings()] for computing good `transform` and `vp_use` settings.
#'          See <https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html>
#'          for more information about the group affine transformation feature.
#'          See [isocubeGrob()] which wraps this function to render isometric cubes.
#' @export
affineGrob <- function(
	grob,
	vp_define = NULL,
	transform = NULL,
	vp_use = NULL,
	name = NULL,
	gp = grid::gpar(),
	vp = NULL
) {
	stopifnot(getRversion() >= "4.2.0")
	if (is.null(transform)) {
		transform <- grid::viewportTransform
	}
	grid::gTree(
		grob = grob,
		vp_define = vp_define,
		transform = transform,
		vp_use = vp_use,
		name = name,
		gp = gp,
		vp = vp,
		cl = "affine"
	)
}

#' @importFrom grid makeContent
#' @export
makeContent.affine <- function(x) {
	if (!isTRUE(grDevices::dev.capabilities()$transformations)) {
		stop(paste(
			"This graphics device does not support the affine transformation feature.",
			"See the Details section of `help(\"affineGrob\")` for more info."
		))
	}
	define <- grid::defineGrob(x$grob, vp = x$vp_define)
	use <- grid::useGrob(define$name, transform = x$transform, vp = x$vp_use)
	gl <- grid::gList(define, use)
	grid::setChildren(x, gl)
}

#' @rdname affineGrob
#' @param ... Passed to `affineGrob()`
#' @export
grid.affine <- function(...) {
	af_grob <- affineGrob(...)
	grid::grid.draw(af_grob)
	invisible(af_grob)
}
