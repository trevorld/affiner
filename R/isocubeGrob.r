#' Isometric cube grob
#'
#' `isometricCube()` is a grid grob function to render
#' isometric cube faces by automatically wrapping around `affineGrob()`.
#'
#' Any `ggplot2` objects are coerced to grobs by [ggplot2::ggplotGrob()].  Depending on what you'd like
#' to do you may want to instead manually convert a ggplot2 object `gg` to a grob with `gtable::gtable_filter(ggplot2::ggplotGrob(gg), "panel")`.
#'
#' `r affine_transformation_support`
#' @param top A grid grob object to use as the top side of the cube.  ggplot2 objects will be coerced by [ggplot2::ggplotGrob()].
#' @param right A grid grob object to use as the right side of the cube.  ggplot2 objects will be coerced by [ggplot2::ggplotGrob()].
#' @param left A grid grob object to use as the left side of the cube.  ggplot2 objects will be coerced by [ggplot2::ggplotGrob()].
#' @param gp_border A [grid::gpar()] object for the [polygonGrob()] used to draw borders around
#'                  the cube faces.
#' @param name A character identifier (for grid).
#' @param gp A [grid::gpar()] object.
#' @param vp A [grid::viewport()] object (or `NULL`).
#' @return A [grid::gTree()] (grob) object of class "isocube".
#'         As a side effect `grid.isocube()` draws to the active graphics device.
#' @examples
#' if (require("grid", quietly = TRUE) &&
#'     getRversion() >= "4.2.0" &&
#'     isTRUE(dev.capabilities()$transformations)) {
#'   # Only works if active graphics device supports affine transformations
#'   # such as `png(type="cairo")` on R 4.2+
#'   grid.newpage()
#'   gp_text <- gpar(fontsize = 72)
#'   grid.isocube(top = textGrob("top", gp = gp_text),
#'                right = textGrob("right", gp = gp_text),
#'                left = textGrob("left", gp = gp_text))
#' }
#' if (require("grid") &&
#'     getRversion() >= "4.2.0" &&
#'     isTRUE(dev.capabilities()$transformations)) {
#'     colors <- c("#D55E00", "#009E73", "#56B4E9")
#'     spacings <- c(0.25, 0.2, 0.25)
#'     texts <- c("pkgname", "left\nface", "right\nface")
#'     rots <- c(45, 0, 0)
#'     fontsizes <- c(52, 80, 80)
#'     sides <- c("top", "left", "right")
#'     types <- gridpattern::names_polygon_tiling[c(5, 7, 9)]
#'     l_grobs <- list()
#'     grid.newpage()
#'     for (i in 1:3) {
#'         if (requireNamespace("gridpattern", quietly = TRUE)) {
#'             bg <- gridpattern::grid.pattern_polygon_tiling(
#'                        colour = "grey80",
#'                        fill = c(colors[i], "white"),
#'                        type = types[i],
#'                        spacing = spacings[i],
#'                        draw = FALSE)
#'         } else {
#'             bg <- rectGrob(gp = gpar(col = NA, fill = colors[i]))
#'         }
#'         text <- textGrob(texts[i], rot = rots[i],
#'                          gp = gpar(fontsize = fontsizes[i]))
#'         l_grobs[[sides[i]]] <- grobTree(bg, text)
#'     }
#'   grid.newpage()
#'   grid.isocube(top = l_grobs$top,
#'                right = l_grobs$right,
#'                left = l_grobs$left)
#' }
#' \donttest{# May take more than 5 seconds on CRAN machines
#' if (require("aRtsy", quietly = TRUE) &&
#'     require("grid") &&
#'     require("ggplot2", quietly = TRUE) &&
#'     requireNamespace("gtable", quietly = TRUE) &&
#'     getRversion() >= "4.2.0" &&
#'     isTRUE(dev.capabilities()$transformations)
#'     ) {
#'   gg <- canvas_planet(colorPalette("lava"), threshold = 3) +
#'     scale_x_continuous(expand=c(0, 0)) +
#'     scale_y_continuous(expand=c(0, 0))
#'   grob <- ggplotGrob(gg)
#'   grob <- gtable::gtable_filter(grob, "panel") # grab just the panel
#'   grid.newpage()
#'   grid.isocube(top = grob, left = grob, right = grob,
#'                gp_border = grid::gpar(col = "darkorange", lwd = 12))
#'
#' }
#' }
#' @export
isocubeGrob <- function(top, right, left,
                        gp_border = grid::gpar(col = "black", lwd = 12),
                        name = NULL, gp = grid::gpar(), vp = NULL) {
    stopifnot(getRversion() >= "4.2.0")
    if (inherits(top, "ggplot"))
        top <- ggplot2::ggplotGrob(top)
    if (inherits(right, "ggplot"))
        right <- ggplot2::ggplotGrob(right)
    if (inherits(left, "ggplot"))
        left <- ggplot2::ggplotGrob(left)

    xy <- as_coord2d(angle(seq(90, 360 + 90, by = 60), "degrees"),
                     radius = c(rep(0.488, 6), 0))
    xy$translate(x = 0.5, y = 0.5)
    l_xy <- list()
    l_xy$top <- xy[c(1, 2, 7, 6)]
    l_xy$right <- xy[c(7, 4, 5, 6)]
    l_xy$left <- xy[c(2, 3, 4, 7)]

    vp_define <- grid::viewport(width = grid::unit(1, "snpc"),
                                height = grid::unit(1, "snpc"))

    grid::gTree(top = top, right = right, left = left,
                gp_border = gp_border, l_xy = l_xy, vp_define = vp_define,
                name = name, gp = gp, vp = vp, cl = "isocube")
}

#' @importFrom grid makeContent
#' @export
makeContent.isocube <- function(x) {
    gl <- grid::gList()
    sides <- c("top", "right", "left")
    for (i in 1:3) {
        side <- sides[[i]]
        xy_side <- x$l_xy[[side]]
        settings <- affine_settings(xy_side, unit = "snpc")
        grob <- x[[side]]
        gl[[i]] <- affineGrob(grob,
                              vp_define = x$vp_define,
                              transform = settings$transform,
                              vp_use = settings$vp)
    }

    x$gp_border$fill <- "transparent"
    for (i in 1:3) {
        side <- sides[[i]]
        xy_side <- x$l_xy[[side]]
        gl[[i + 3L]] <- grid::polygonGrob(xy_side$x, xy_side$y, gp = x$gp_border,
                                          default.units = "snpc")
    }

    grid::setChildren(x, gl)
}

#' @rdname isocubeGrob
#' @param ... Passed to `isocubeGrob()`
#' @export
grid.isocube <- function(...) {
    ic_grob <- isocubeGrob(...)
    grid::grid.draw(ic_grob)
    invisible(ic_grob)
}
