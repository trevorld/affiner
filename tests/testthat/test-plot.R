test_that("`plot()` and `autolayer()` methods", {
	skip_if_not_installed("ggplot2")
	skip_if_not_installed("vdiffr")
	suppressPackageStartupMessages(library("ggplot2"))
	library("vdiffr")

	p1 <- as_coord1d(x = seq.int(-4, -1))
	pt <- as_point1d(a = 1, b = 0)
	p2 <- p1$clone()$reflect(pt)
	expect_doppelganger("plot.Coord1D", function() {
		plot(p1, xlim = c(-5, 5))
		lines(pt)
		points(p2, col = "red")
	})
	expect_doppelganger("autolayer.Coord1D", {
		ggplot() +
			autolayer(p1) +
			autolayer(pt) +
			autolayer(p2, color = "red")
	})

	p1 <- as_coord2d(x = 0, y = 1:10)
	l <- as_line2d(a = 1, b = -1, c = 0)
	p2 <- p1$clone()$reflect(l)
	expect_doppelganger("plot.Coord2D reflect", function() {
		plot(p1, xlim = c(-1, 11), ylim = c(-1, 11))
		lines(l)
		points(p2, col = "red")
	})
	expect_doppelganger("autolayer.Coord2D reflect", {
		ggplot() +
			autolayer(p1) +
			autolayer(l) +
			autolayer(p2, color = "red")
	})

	p1 <- as_coord2d(x = 1:10, y = 1:10)
	l <- as_line2d(a = -1, b = 0, c = 0)
	p2 <- p1$clone()$project(l)
	expect_doppelganger("plot.Coord2D project", function() {
		plot(p1, xlim = c(-1, 11))
		lines(l)
		points(p2, col = "red")
	})
	expect_doppelganger("autolayer.Coord2D project", {
		ggplot() +
			autolayer(p1) +
			autolayer(l) +
			autolayer(p2, color = "red")
	})
})

test_that("lines.Coord2D, plot.Polygon2D, lines/plot.Ellipse2D", {
	skip_if_not_installed("vdiffr")
	library("vdiffr")

	pts <- as_coord2d(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0))
	sq <- as_polygon2d(as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)))
	circ <- as_ellipse2d(as_coord2d(0.5, 0.5), r = 0.4)
	ell <- as_ellipse2d(as_coord2d(0.5, 0.5), rx = 0.4, ry = 0.2, theta = pi / 6)

	expect_doppelganger("lines.Coord2D", function() {
		plot(pts, xlim = c(0, 1), ylim = c(0, 1))
		lines(pts)
	})
	expect_doppelganger("lines.Polygon2D", function() {
		plot(pts, xlim = c(0, 1), ylim = c(0, 1))
		lines(sq)
	})
	expect_doppelganger("plot.Polygon2D", function() {
		plot(sq, col = "lightblue")
	})
	expect_doppelganger("lines.Ellipse2D circle", function() {
		plot(sq, col = "lightblue")
		lines(circ, col = "red")
	})
	expect_doppelganger("lines.Ellipse2D ellipse", function() {
		plot(sq, col = "lightblue")
		lines(ell, col = "darkgreen")
	})
	expect_doppelganger("plot.Ellipse2D circle", function() {
		plot(circ, col = "lightyellow")
	})
	expect_doppelganger("plot.Ellipse2D multiple", function() {
		e2 <- as_ellipse2d(x = c(0, 1), y = c(0, 1), rx = c(0.4, 0.3), ry = c(0.2, 0.3))
		plot(e2, col = "lightblue")
	})
})

test_that("autolayer.Polygon2D and autolayer.Ellipse2D", {
	skip_if_not_installed("ggplot2")
	skip_if_not_installed("vdiffr")
	suppressPackageStartupMessages(library("ggplot2"))
	library("vdiffr")

	sq <- as_polygon2d(as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)))
	circ <- as_ellipse2d(as_coord2d(0.5, 0.5), r = 0.4)
	ell <- as_ellipse2d(as_coord2d(0.5, 0.5), rx = 0.4, ry = 0.2, theta = pi / 6)

	expect_doppelganger("autolayer.Polygon2D", {
		ggplot() + autolayer(sq, fill = "lightblue")
	})
	expect_doppelganger("autolayer.Ellipse2D circle", {
		ggplot() + autolayer(circ, fill = "lightyellow")
	})
	expect_doppelganger("autolayer.Ellipse2D multiple", {
		e2 <- as_ellipse2d(x = c(0, 1), y = c(0, 1), rx = c(0.4, 0.3), ry = c(0.2, 0.3))
		ggplot() + autolayer(e2, fill = "lightblue")
	})
})

test_that("rgl works", {
	skip_on_cran()
	skip_if_not_installed("rgl")
	rgl::open3d(useNULL = TRUE)
	c1 <- as_coord3d(x = 1:10, y = 1:10, z = 1:10)
	pl <- as_plane3d(a = 0, b = 0, c = -1, d = 2) # z = 2
	c2 <- c1$clone()$reflect(pl)
	rgl::plot3d(c1, size = 8)
	rgl::plot3d(pl, color = "grey", alpha = 0.6)
	rgl::points3d(as.data.frame(c2), col = "red", size = 8)
	expect_true(nrow(rgl::ids3d()) >= 6L)
	rgl::close3d()
})
