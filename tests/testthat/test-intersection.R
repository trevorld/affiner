test_that("`has_intersection()`", {
	line1 <- as_line2d("x-axis")
	line2 <- as_line2d("y-axis")
	line3 <- as_line2d(a = 0, b = 1, c = 2) # y + 2 = 0
	expect_true(has_intersection(line1, line1))
	expect_true(has_intersection(line1, line2))
	expect_true(has_intersection(line1, "y-axis"))
	expect_false(has_intersection(line1, line3))

	plane1 <- as_plane3d("xy-plane")
	plane2 <- as_plane3d("xz-plane")
	plane3 <- as_plane3d(a = 0, b = 0, c = 1, d = 2) # z + 2 = 0
	expect_true(has_intersection(plane1, plane1))
	expect_true(has_intersection(plane1, plane2))
	expect_true(has_intersection(plane1, "xz-plane"))
	expect_false(has_intersection(plane1, plane3))

	point1d <- as_point1d("origin")
	point2d <- as_point1d(a = 1, b = 2)
	expect_true(has_intersection(point1d, "origin"))
	expect_false(has_intersection(point1d, point2d))

	p1 <- as_coord1d("origin")
	p2 <- as_coord1d(x = 3)
	expect_true(has_intersection(p1, "origin"))
	expect_false(has_intersection(p1, p2))

	p1 <- as_coord2d("origin")
	p2 <- as_coord2d(x = 3)
	expect_true(has_intersection(p1, "origin"))
	expect_false(has_intersection(p1, p2))

	p1 <- as_coord3d("origin")
	p2 <- as_coord3d(x = 3)
	expect_true(has_intersection(p1, "origin"))
	expect_false(has_intersection(p1, p2))

	line <- as_line2d("x-axis")
	coord <- as_coord2d(x = 0, y = 0:2)
	expect_equal(has_intersection(line, coord), c(TRUE, FALSE, FALSE))
	expect_equal(has_intersection(coord, line), c(TRUE, FALSE, FALSE))

	plane <- as_plane3d("xy-plane")
	coord <- as_coord3d(x = 0, y = 0, z = 0:2)
	expect_equal(has_intersection(plane, coord), c(TRUE, FALSE, FALSE))
	expect_equal(has_intersection(coord, plane), c(TRUE, FALSE, FALSE))
})

test_that("`intersection()`", {
	line1 <- as_line2d("x-axis")
	line2 <- as_line2d("y-axis")
	line3 <- as_line2d(a = 0, b = 1, c = 2) # y + 2 = 0
	line4 <- as_line2d(a = 1, b = 0, c = 2) # x + 2 = 0
	expect_equal(intersection(line1, line1), list(standardize(line1)))
	expect_equal(intersection(line1, line2), list(as_coord2d("origin")))
	expect_equal(intersection(line1, "y-axis"), list(as_coord2d("origin")))
	expect_equal(intersection(line1, line3), list(NULL))
	expect_equal(intersection(line3, line4), list(as_coord2d(x = -2, y = -2)))

	plane1 <- as_plane3d("xy-plane")
	plane2 <- as_plane3d("xz-plane")
	plane3 <- as_plane3d(a = 0, b = 0, c = 1, d = 2) # z + 2 = 0
	expect_equal(intersection(plane1, plane1), list(standardize(plane1)))
	expect_error(intersection(plane1, plane2))
	expect_error(intersection(plane1, "xz-plane"))
	expect_equal(intersection(plane1, plane3), list(NULL))

	point1 <- as_point1d("origin")
	point2 <- as_point1d(a = 1, b = 2)
	expect_equal(intersection(point1, "origin"), list(standardize(point1)))
	expect_equal(intersection(point1, point2), list(NULL))

	p1 <- as_coord1d("origin")
	p2 <- as_coord1d(x = 3)
	expect_equal(intersection(p1, "origin"), list(p1))
	expect_equal(intersection(p1, p2), list(NULL))

	p1 <- as_coord2d("origin")
	p2 <- as_coord2d(x = 3)
	expect_equal(intersection(p1, "origin"), list(p1))
	expect_equal(intersection(p1, p2), list(NULL))

	p1 <- as_coord3d("origin")
	p2 <- as_coord3d(x = 3)
	expect_equal(intersection(p1, "origin"), list(p1))
	expect_equal(intersection(p1, p2), list(NULL))
})

test_that("intersection() Line2D and Ellipse2D", {
	circ <- as_ellipse2d(as_coord2d("origin"), r = 1)
	xax <- as_line2d("x-axis") # y = 0

	# Two crossing points: (1, 0) and (-1, 0)
	res <- intersection(circ, xax)
	expect_equal(length(res), 1L)
	expect_equal(length(res[[1L]]), 2L)
	expect_equal(sort(res[[1L]]$x), c(-1, 1), tolerance = 1e-10)
	expect_equal(res[[1L]]$y, c(0, 0), tolerance = 1e-10)

	# Both dispatch directions give same result
	expect_equal(intersection(xax, circ), res)

	# Tangent: y = 1 touches top of unit circle at (0, 1)
	line_top <- as_line2d(a = 0, b = 1, c = -1)
	res_tan <- intersection(circ, line_top)
	expect_equal(length(res_tan[[1L]]), 1L)
	expect_equal(res_tan[[1L]]$x, 0, tolerance = 1e-10)
	expect_equal(res_tan[[1L]]$y, 1, tolerance = 1e-10)

	# No intersection: y = 2 is above the unit circle
	line_above <- as_line2d(a = 0, b = 1, c = -2)
	expect_equal(intersection(circ, line_above), list(NULL))

	# Rotated ellipse: center (0,0), rx=2, ry=1, theta=0 vs x-axis
	e <- as_ellipse2d(as_coord2d(0, 0), rx = 2, ry = 1)
	res_e <- intersection(e, xax)
	expect_equal(length(res_e[[1L]]), 2L)
	expect_equal(sort(res_e[[1L]]$x), c(-2, 2), tolerance = 1e-10)
	expect_equal(res_e[[1L]]$y, c(0, 0), tolerance = 1e-10)

	# has_intersection uses intersection() internally
	expect_true(has_intersection(circ, xax))
	expect_false(has_intersection(circ, line_above))
})
