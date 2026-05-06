test_that("painter_depth.Coord2D() orthographic gives zero depth", {
	p <- as_coord2d(x = 1:3, y = 1:3)
	expect_equal(painter_depth(p), rep(0, 3))
})

test_that("painter_depth.Coord2D() oblique matches formula", {
	p <- as_coord2d(x = c(1, 2), y = c(1, 1))
	s <- 0.5
	a <- degrees(45)
	expected <- -s * cos(a) * p$x - s * sin(a) * p$y
	expect_equal(painter_depth(p, scale = s, alpha = a), expected)
})

test_that("painter_depth.Coord3D() orthographic onto xy-plane gives z", {
	p <- as_coord3d(x = 1:3, y = 1:3, z = c(3, 1, 2))
	expect_equal(painter_depth(p), p$z)
})

test_that("painter_depth.Coord3D() oblique onto xy-plane matches formula", {
	p <- as_coord3d(x = c(1, 2), y = c(1, 1), z = c(0, 1))
	s <- 0.5
	a <- degrees(45)
	expected <- -s * cos(a) * p$x - s * sin(a) * p$y + p$z
	expect_equal(painter_depth(p, scale = s, alpha = a), expected)
})

test_that("painter_depth.Coord3D() respects non-default plane", {
	p <- as_coord3d(x = c(1, 2), y = c(3, 4), z = c(5, 6))
	d <- painter_depth(p, plane = "xz-plane")
	expect_equal(d, -p$y)
})

test_that("painter_depth.Coord3D() roll rotates in-plane frame, leaving depth unchanged", {
	p <- as_coord3d(x = c(1, 2), y = c(1, 1), z = c(2, 3))
	d0 <- painter_depth(p)
	d_roll <- painter_depth(p, roll = degrees(90))
	expect_equal(d0, p$z)
	expect_equal(d_roll, p$z)
})

test_that("painter_depth.Segment2D() uses midpoints", {
	p1 <- as_coord2d(x = c(0, 2), y = c(0, 0))
	p2 <- as_coord2d(x = c(2, 2), y = c(0, 2))
	s <- as_segment2d(p1, p2 = p2)
	expect_equal(painter_depth(s), painter_depth(s$mid_point))
})

test_that("painter_depth.Polygon2D() gives one depth per edge", {
	vertices <- as_coord2d(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0))
	poly <- as_polygon2d(vertices)
	d <- painter_depth(poly, scale = 0.5, alpha = degrees(45))
	expect_length(d, 4L)
	expect_equal(d, painter_depth(poly$edges, scale = 0.5, alpha = degrees(45)))
})
