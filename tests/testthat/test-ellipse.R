test_that("as_ellipse2d()", {
	# from Coord2D
	e <- as_ellipse2d(as_coord2d(0, 0), rx = 2, ry = 1, theta = degrees(45))
	expect_r6_class(e, "Ellipse2D")
	expect_true(is_ellipse2d(e))
	expect_true(is_coord2d(e))
	expect_equal(length(e), 1L)
	expect_equal(e$rx, 2)
	expect_equal(e$ry, 1)
	expect_true(is_angle(e$theta))
	expect_equal(as.numeric(e$theta, "degrees"), 45)
	expect_false(e$is_circle)

	# from numeric
	e2 <- as_ellipse2d(x = c(0, 1), y = c(0, 1), rx = c(1, 2), ry = c(0.5, 1))
	expect_equal(length(e2), 2L)

	# recycling: one center, two radii
	e3 <- as_ellipse2d(as_coord2d(0, 0), rx = c(1, 2), ry = c(0.5, 1))
	expect_equal(length(e3), 2L)

	# [.angle preserves class when indexing theta
	expect_true(is_angle(e3$theta[1L]))

	# print
	expect_snapshot(print(e))
})

test_that("mixed circle/non-circle ellipses()", {
	e1 <- as_ellipse2d(as_coord2d(0, 0), rx = 2, ry = 1)
	expect_true(is_ellipse2d(e1))
	# mixed vector: not all circles → not all is_circle
	mixed <- as_ellipse2d(x = c(0, 1), y = c(0, 0), rx = c(1, 1), ry = c(1, 0.5))
	expect_false(all(mixed$is_circle))
})

test_that("Ellipse2D$is_circle active binding", {
	e <- as_ellipse2d(x = c(0, 1), y = c(0, 0), rx = c(1, 2), ry = c(1, 1))
	expect_equal(e$is_circle, c(TRUE, FALSE))
})

test_that("Ellipse2D transform() updates shape parameters", {
	# Uniform scaling: rx and ry both scaled
	c1 <- as_ellipse2d(as_coord2d(0, 0), r = 1)
	c1$scale(2)
	expect_equal(c1$rx, 2)
	expect_equal(c1$ry, 2)
	expect_true(c1$is_circle)

	# Non-uniform scaling turns circle into ellipse
	c2 <- as_ellipse2d(as_coord2d(0, 0), r = 1)
	c2$scale(2, 1)
	expect_equal(c2$rx, 2)
	expect_equal(c2$ry, 1)
	expect_false(c2$is_circle)

	# Rotation of a circle stays a circle; theta is still an angle
	c3 <- as_ellipse2d(as_coord2d(1, 0), r = 1)
	c3$rotate(degrees(45))
	expect_true(c3$is_circle)
	expect_equal(c3$rx, 1, tolerance = 1e-10)
	expect_equal(c3$ry, 1, tolerance = 1e-10)
	expect_true(is_angle(c3$theta))

	# Translation only moves center, not radii
	c4 <- as_ellipse2d(as_coord2d(0, 0), r = 1)
	c4$translate(as_coord2d(3, 4))
	expect_equal(c4$x, 3)
	expect_equal(c4$y, 4)
	expect_equal(c4$rx, 1)
})

test_that("as.list.Ellipse2D() and as.data.frame.Ellipse2D()", {
	e <- as_ellipse2d(as_coord2d(1, 2), rx = 3, ry = 2, theta = degrees(30))
	lst <- as.list(e)
	expect_equal(lst$x, 1)
	expect_equal(lst$y, 2)
	expect_equal(lst$rx, 3)
	expect_equal(lst$ry, 2)
	expect_true(is_angle(lst$theta))
	expect_equal(as.numeric(lst$theta, "degrees"), 30)

	df <- as.data.frame(e)
	expect_equal(names(df), c("x", "y", "rx", "ry", "theta"))
	expect_true(is_angle(df$theta))
})

test_that("as_polygon2d.Ellipse2D()", {
	c1 <- as_ellipse2d(as_coord2d(0, 0), r = 1)

	inner <- as_polygon2d(c1, n = 8L, type = "inner")
	expect_r6_class(inner, "Polygon2D")
	expect_true(inner$is_convex)
	expect_equal(length(inner), 8L)
	# inner polygon vertices lie on the circle: abs should equal 1
	expect_equal(abs(inner - as_coord2d(0, 0)), rep(1, 8L), tolerance = 1e-10)

	outer <- as_polygon2d(c1, n = 8L, type = "outer")
	expect_r6_class(outer, "Polygon2D")
	expect_true(outer$is_convex)
	expect_equal(length(outer), 8L)
	# outer polygon vertices lie outside the circle
	expect_true(all(abs(outer - as_coord2d(0, 0)) > 1))

	# inner contains circle, outer contains inner
	# (all inner vertices closer than all outer vertices to center)
	expect_true(
		all(abs(inner - as_coord2d(0, 0)) < abs(outer - as_coord2d(0, 0)))
	)

	# only length-1 Ellipse2D supported
	e2 <- as_ellipse2d(x = c(0, 1), y = c(0, 0), rx = c(1, 2), ry = c(1, 1))
	expect_error(as_polygon2d(e2))
})

test_that("has_overlap2d() circle-circle", {
	c1 <- as_ellipse2d(as_coord2d(0, 0), r = 1)
	c2 <- as_ellipse2d(as_coord2d(1.5, 0), r = 1)
	c3 <- as_ellipse2d(as_coord2d(3, 0), r = 1)

	expect_true(has_overlap2d(c1, c2))
	expect_false(has_overlap2d(c1, c3))

	# Tangent circles: no non-zero-area overlap
	c4 <- as_ellipse2d(as_coord2d(2, 0), r = 1)
	expect_false(has_overlap2d(c1, c4))

	# Vectorized
	c_many <- as_ellipse2d(x = c(1.5, 3), y = c(0, 0), r = c(1, 1))
	res <- has_overlap2d(c1, c_many)
	expect_equal(res, c(TRUE, FALSE))
})

test_that("has_overlap2d() ellipse-polygon (circle case uses SAT)", {
	sq <- as_polygon2d(as_coord2d(
		x = c(0, 1, 1, 0),
		y = c(0, 0, 1, 1)
	))
	circ_in <- as_ellipse2d(as_coord2d(0.5, 0.5), r = 0.3)
	circ_out <- as_ellipse2d(as_coord2d(3, 3), r = 0.3)

	expect_true(has_overlap2d(sq, circ_in))
	expect_false(has_overlap2d(sq, circ_out))

	# Dispatching from Ellipse2D side
	expect_true(has_overlap2d(circ_in, sq))
	expect_false(has_overlap2d(circ_out, sq))

	# Vectorized
	circs <- as_ellipse2d(x = c(0.5, 3), y = c(0.5, 3), r = c(0.3, 0.3))
	expect_equal(has_overlap2d(sq, circs), c(TRUE, FALSE))
})

test_that("has_overlap2d() non-circle ellipse uses bracketing", {
	sq <- as_polygon2d(as_coord2d(
		x = c(0, 1, 1, 0),
		y = c(0, 0, 1, 1)
	))
	# Clearly inside
	e_in <- as_ellipse2d(as_coord2d(0.5, 0.5), rx = 0.3, ry = 0.1)
	expect_true(has_overlap2d(e_in, sq))

	# Clearly outside
	e_out <- as_ellipse2d(as_coord2d(5, 5), rx = 0.3, ry = 0.1)
	expect_false(has_overlap2d(e_out, sq))
})
