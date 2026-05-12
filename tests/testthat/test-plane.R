test_that("as_plane3d()", {
	p1 <- as_plane3d(1, 2, 3, 4)
	expect_true(is_plane3d(p1))

	expect_equal(as_plane3d("xy-plane"), as_plane3d(0, 0, 1, 0))
	expect_true(is_equivalent(as_plane3d(0, 0, 1, 0), "xy-plane"))

	expect_equal(p1, as_plane3d(p1))
	expect_equal(p1, p1[1L])
	expect_warning(as_plane3d("boo"))

	expect_equal(as_plane3d("xy-plane"), as_plane3d(normal3d("xy-plane"), "origin"))

	expect_equal(as_plane3d(as_line2d(1, 0, -2)), as_plane3d(1, 0, 0, -2))

	expect_false(is.na(p1))
	expect_false(is.nan(p1))
	expect_false(is.infinite(p1))
	expect_true(is.finite(p1))
	expect_length(p1, 1L)
	expect_length(rep(p1, 3), 3L)
	expect_length(c(p1, p1), 2L)
	expect_true(is.data.frame(as.data.frame(p1)))
	expect_true(is.list(as.list(p1)))

	expect_snapshot(print(p1))

	expect_equal(
		as_plane3d(p1 = as_coord3d("origin"), p2 = "x-axis", p3 = "y-axis"),
		as_plane3d("xy-plane")
	)

	pt <- as_point1d(a = 1, b = -1)
	expect_equal(as_plane3d(pt, b = 3, c = 4), as_plane3d(a = 1, b = 3, c = 4, d = -1))
})

test_that("range.Plane3D()", {
	# x-perpendicular planes: x is finite, y and z are infinite
	xp <- as_plane3d(a = c(1, 1), b = c(0, 0), c = c(0, 0), d = c(-2, -5))
	r <- range(xp)
	expect_equal(r$x, c(2, 5))
	expect_equal(r$y, c(-Inf, Inf))
	expect_equal(r$z, c(-Inf, Inf))

	# y-perpendicular plane: y is finite, x and z are infinite
	yp <- as_plane3d(a = 0, b = 1, c = 0, d = -4)
	r2 <- range(yp)
	expect_equal(r2$x, c(-Inf, Inf))
	expect_equal(r2$y, c(4, 4))
	expect_equal(r2$z, c(-Inf, Inf))

	# General plane: all dimensions are infinite
	gp <- as_plane3d(a = 1, b = 1, c = 1, d = 0)
	r3 <- range(gp)
	expect_equal(r3$x, c(-Inf, Inf))
	expect_equal(r3$y, c(-Inf, Inf))
	expect_equal(r3$z, c(-Inf, Inf))
})
