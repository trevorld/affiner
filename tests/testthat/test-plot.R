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

test_that("rgl works", {
	skip_on_cran()
	skip_if_not_installed("rgl")
	rgl::open3d(useNULL = TRUE)
	c1 <- as_coord3d(x = 1:10, y = 1:10, z = 1:10)
	pl <- as_plane3d(a = 0, b = 0, c = -1, d = 2) # z = 2
	c2 <- c1$clone()$reflect(pl)
	rgl::plot3d(c1, size = 8)
	rgl::planes3d(as.data.frame(pl), d = pl$d, color = "grey", alpha = 0.6)
	rgl::points3d(as.data.frame(c2), col = "red", size = 8)
	expect_true(nrow(rgl::ids3d()) >= 6L)
	rgl::close3d()
})
