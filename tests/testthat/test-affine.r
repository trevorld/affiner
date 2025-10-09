test_that("transform1d()", {
	m <- transform1d(diag(2L))
	expect_true(is_transform1d(m))
	expect_equal(m, as_transform1d(m))
	expect_equal(m, as_transform1d(diag(1L)))
	expect_error(as_transform1d(t(translate1d(as_coord1d(1)))))
	expect_error(as_transform1d(m + 2))

	x <- c(2, 5, 7)
	p1 <- as_coord1d(x = x)

	expect_equal(p1, p1$transform()$transform(diag(2L)))

	skip_if_not(getRversion() >= "4.3.0")
	expect_true(is_transform1d(m %*% m))
})

test_that("transform2d()", {
	m <- transform2d(diag(3))
	expect_true(is_transform2d(m))
	expect_equal(m, as_transform2d(m))
	expect_equal(m, as_transform2d(diag(2)))

	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	p1 <- as_coord2d(x = x, y = y)

	expect_equal(p1, p1$transform()$transform(diag(3)))

	skip_if_not(getRversion() >= "4.3.0")
	expect_true(is_transform2d(m %*% m))
})

test_that("transform3d()", {
	m <- transform3d(diag(4))
	expect_true(is_transform3d(m))
	expect_true(is_transform3d(solve(m)))
	expect_error(as_transform3d(t(translate3d(as_coord3d(1, 2, 3)))))
	expect_error(as_transform3d(m + 2))
	expect_equal(m, as_transform3d(m))
	expect_equal(m, as_transform3d(diag(3)))

	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	z <- c(4, 9, 3)
	p1 <- as_coord3d(x = x, y = y, z = z)

	expect_equal(p1, p1$transform()$transform(diag(4)))

	skip_if_not(getRversion() >= "4.3.0")
	expect_true(is_transform3d(m %*% m))
})

test_that("at_matrix methods", {
	m <- transform2d(diag(3))
	expect_true(is_transform2d(solve(m)))
	expect_false(is_transform2d(as.matrix(m)))
	expect_false(is_transform2d(t(m)))
	expect_false(is_transform2d(m + 2))
	expect_error(as_transform2d(m + 2))
	expect_false(is_transform2d(m + m))
	expect_false(is_transform2d(abs(m)))
	expect_false(is_transform2d(Re(m)))

	expect_error(as_transform2d(t(translate2d(x = 2, y = 2))))
})

test_that("permute2d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	p <- as_coord2d(x, y)
	expect_equal(p, p$clone()$permute())
	expect_equal(as_coord2d(y, x), p$clone()$permute("yx"))
})

test_that("permute3d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	z <- c(4, 9, 3)
	p <- as_coord3d(x, y, z)
	expect_equal(p, p$clone()$permute())
	expect_equal(as_coord3d(x, z, y), p$clone()$permute("xzy"))
	expect_equal(as_coord3d(y, x, z), p$clone()$permute("yxz"))
	expect_equal(as_coord3d(y, z, x), p$clone()$permute("yzx"))
	expect_equal(as_coord3d(z, x, y), p$clone()$permute("zxy"))
	expect_equal(as_coord3d(z, y, x), p$clone()$permute("zyx"))
})

test_that("project1d()", {
	x <- c(2, 5, 7)
	p1 <- as_coord1d(x = x)$project(as_coord1d(x = 3))
	expect_equal(p1$x, rep(3, 3))
})

test_that("project2d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	p1 <- as_coord2d(x = x, y = y)$project(as_line2d("x-axis"))
	expect_equal(p1$x, x)
	expect_equal(p1$y, rep(0, 3))

	p2 <- as_coord2d(x = x, y = y)$project(degrees(90))
	expect_equal(p2$x, rep(0, 3))
	expect_equal(p2$y, y)

	p3 <- as_coord2d(x = x, y = y)$project(scale = 0.5)
	expect_equal(p3$x, x + 0.5 * y)
	expect_equal(p3$y, rep(0, 3))

	l4 <- as_line2d(a = 0, b = 1, c = -4)
	p4 <- as_coord2d(x = x, y = y)$project(l4)
	expect_equal(p4$x, x)
	expect_equal(p4$y, rep(4, 3))

	l5 <- as_line2d(a = 1, b = 0, c = -4)
	p5 <- as_coord2d(x = x, y = y)$project(l5)
	expect_equal(p5$x, rep(4, 3))
	expect_equal(p5$y, y)

	x6 <- c(-5, 5, 5)
	y6 <- c(5, -5, 5)
	l6 <- as_line2d(a = 1, b = -1, c = 0)
	p6 <- as_coord2d(x = x6, y = y6)$project(l6)
	expect_equal(p6$x, c(0, 0, 5))
	expect_equal(p6$y, c(0, 0, 5))

	l7 <- as_line2d(a = 1, b = -1, c = 2)
	p7 <- as_coord2d(x = x6, y = y6)$project(l7)
	expect_equal(p7$x, c(-1, -1, 4))
	expect_equal(p7$y, c(1, 1, 6))
})

test_that("project3d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	z <- c(4, 9, 3)
	p1 <- as_coord3d(x, y, z)$project()
	expect_equal(p1$x, x)
	expect_equal(p1$y, y)
	expect_equal(p1$z, rep(0, 3))

	p2 <- as_coord3d(x, y, z)$project("xz-plane")
	expect_equal(p2$x, x)
	expect_equal(p2$y, rep(0, 3))
	expect_equal(p2$z, z)

	p3 <- as_coord3d(x, y, z)$project("yz-plane")
	expect_equal(p3$x, rep(0, 3))
	expect_equal(p3$y, y)
	expect_equal(p3$z, z)

	alpha <- angle(60, "degrees")
	scale <- 0.5
	p4 <- as_coord3d(x, y, z)$project(scale = scale, alpha = alpha)
	expect_equal(p4$x, x + scale * cos(alpha) * z)
	expect_equal(p4$y, y + scale * sin(alpha) * z)
	expect_equal(p4$z, rep(0, 3))

	p42 <- as_coord3d(x, y, z)$project(scale = scale, alpha = 60, unit = "degrees")

	p5 <- as_coord3d(x, y, z)$permute("xzy")$project(scale = scale, alpha = alpha)
	expect_equal(p5$x, x + scale * cos(alpha) * y)
	expect_equal(p5$y, z + scale * sin(alpha) * y)
	expect_equal(p5$z, rep(0, 3))

	# Planes not on origin
	plane <- as_plane3d(a = 0, b = 0, c = 1, d = -2)
	p6 <- as_coord3d(x, y, z)$project(plane = plane, scale = scale, alpha = alpha)
	expect_equal(p6$x, x + scale * cos(alpha) * (z - 2))
	expect_equal(p6$y, y + scale * sin(alpha) * (z - 2))
	expect_equal(p6$z, rep(2, 3))

	# Plane through origin, not axis-aligned
	pl10 <- as_plane3d(a = -1, b = -1, c = 1, d = 0)
	p10 <- as_coord3d(x = -5, y = -5, z = 5)$project(pl10)
	expect_equal(p10$x, 0)
	expect_equal(p10$y, 0)
	expect_equal(p10$z, 0)
})

test_that("reflect1d()", {
	x <- c(2, 5, 7)
	p1 <- as_coord1d(x = x)$reflect("origin")
	expect_equal(p1$x, -x)
	p2 <- as_coord1d(x = x)$reflect(as_coord1d(x = 5))
	expect_equal(p2$x, c(8, 5, 3))
})

test_that("reflect2d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	p1 <- as_coord2d(x = x, y = y)$reflect("x-axis")
	expect_equal(p1$x, x)
	expect_equal(p1$y, -y)

	p1 <- as_coord2d(x = x, y = y)$reflect(as_line2d(a = 0, b = 1, c = 0))
	expect_equal(p1$x, x)
	expect_equal(p1$y, -y)

	p2 <- as_coord2d(x = x, y = y)$reflect("y-axis")
	expect_equal(p2$x, -x)
	expect_equal(p2$y, y)

	p2 <- as_coord2d(x = x, y = y)$reflect(as_line2d(a = 1, b = 0, c = 0))
	expect_equal(p2$x, -x)
	expect_equal(p2$y, y)

	p3 <- as_coord2d(x = x, y = y)$reflect(degrees(135))
	expect_equal(p3$x, -y)
	expect_equal(p3$y, -x)
	p4 <- as_coord2d(x = x, y = y)$reflect(as_coord2d(1, 1))
	expect_equal(p4$x, -y)
	expect_equal(p4$y, -x)

	lh <- as_line2d(a = 0, b = 1, c = -2) # y = 2
	p5 <- as_coord2d(x = x, y = y)$reflect(lh)
	expect_equal(p5$x, x)
	expect_equal(p5$y, 2 - (y - 2))

	lv <- as_line2d(a = 1, b = 0, c = -2) # x = 2
	p6 <- as_coord2d(x = x, y = y)$reflect(lv)
	expect_equal(p6$x, 2 - (x - 2))
	expect_equal(p6$y, y)

	x6 <- c(-5, 5, 5)
	y6 <- c(5, -5, 5)
	l7 <- as_line2d(a = 1, b = -1, c = 2)
	p7 <- as_coord2d(x = x6, y = y6)$reflect(l7)
	expect_equal(p7$x, c(3, -7, 3))
	expect_equal(p7$y, c(-3, 7, 7))
})

test_that("reflect3d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	z <- c(4, 9, 3)
	p1 <- as_coord3d(x = x, y = y, z = z)$reflect("xy-plane")
	expect_equal(p1$x, x)
	expect_equal(p1$y, y)
	expect_equal(p1$z, -z)

	p1a <- as_coord3d(x = x, y = y, z = z)$reflect(as_plane3d(a = 0, b = 0, c = 1, d = -2))
	expect_equal(p1a$x, x)
	expect_equal(p1a$y, y)
	expect_equal(p1a$z, 2 - (z - 2))

	p2 <- as_coord3d(x = x, y = y, z = z)$reflect("xz-plane")
	expect_equal(p2$x, x)
	expect_equal(p2$y, -y)
	expect_equal(p2$z, z)

	p2a <- as_coord3d(x = x, y = y, z = z)$reflect(as_plane3d(a = 0, b = 1, c = 0, d = -2))
	expect_equal(p2a$x, x)
	expect_equal(p2a$y, 2 - (y - 2))
	expect_equal(p2a$z, z)

	p3 <- as_coord3d(x = x, y = y, z = z)$reflect("yz-plane")
	expect_equal(p3$x, -x)
	expect_equal(p3$y, y)
	expect_equal(p3$z, z)

	p3a <- as_coord3d(x = x, y = y, z = z)$reflect(as_plane3d(a = 1, b = 0, c = 0, d = -2))
	expect_equal(p3a$x, 2 - (x - 2))
	expect_equal(p3a$y, y)
	expect_equal(p3a$z, z)

	p4 <- as_coord3d(x = x, y = y, z = z)$reflect(as_coord3d(1, 0, 1))
	expect_equal(p4$x, -z)
	expect_equal(p4$y, y)
	expect_equal(p4$z, -x)

	# Planes not on origin
	plh <- as_plane3d(a = 0, b = 1, c = 0, d = -2) # y = 2
	p5 <- as_coord3d(x = x, y = y, z = 0)$reflect(plh)
	expect_equal(p5$x, x)
	expect_equal(p5$y, 2 - (y - 2))
	expect_equal(p5$z, rep_len(0, 3))

	plv <- as_plane3d(a = 1, b = 0, c = 0, d = -2) # x = 2
	p6 <- as_coord3d(x = x, y = y, z = 0)$reflect(plv)
	expect_equal(p6$x, 2 - (x - 2))
	expect_equal(p6$y, y)
	expect_equal(p6$z, rep_len(0, 3))

	# Planes not on origin, not axis-aligned
	x6 <- c(-5, 5, 5)
	y6 <- c(5, -5, 5)
	pl7 <- as_plane3d(a = 1, b = -1, c = 0, d = 2) # y = x + 2
	p7 <- as_coord3d(x = x6, y = y6, z = 0)$reflect(pl7)
	expect_equal(p7$x, c(3, -7, 3))
	expect_equal(p7$y, c(-3, 7, 7))
	expect_equal(p7$z, rep_len(0, 3))

	pl8 <- as_plane3d(a = 1, b = 0, c = -1, d = 2) # z = x + 2
	p8 <- as_coord3d(x = x6, y = 0, z = y6)$reflect(pl8)
	expect_equal(p8$x, c(3, -7, 3))
	expect_equal(p8$y, rep_len(0, 3))
	expect_equal(p8$z, c(-3, 7, 7))

	pl9 <- as_plane3d(a = 0, b = -1, c = 1, d = 2) # y = z + 2
	p9 <- as_coord3d(x = 0, y = y6, z = x6)$reflect(pl9)
	expect_equal(p9$x, rep_len(0, 3))
	expect_equal(p9$y, c(-3, 7, 7))
	expect_equal(p9$z, c(3, -7, 3))

	# Plane through origin, not axis-aligned
	pl10 <- as_plane3d(a = -1, b = -1, c = 1, d = 0)
	p10 <- as_coord3d(x = -5, y = -5, z = 5)$reflect(pl10)
	expect_equal(p10$x, 5)
	expect_equal(p10$y, 5)
	expect_equal(p10$z, -5)
})

test_that("rotate2d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	p1 <- as_coord2d(x = x, y = y)$rotate(90, "degrees")
	expect_equal(p1$x, c(-3, -4, -6))
	expect_equal(p1$y, c(2, 5, 7))

	p2 <- as_coord2d(x = x, y = y)$rotate(-0.5, "pi-radians")
	expect_equal(p2$x, c(3, 4, 6))
	expect_equal(p2$y, c(-2, -5, -7))

	p3 <- as_coord2d(x = x, y = y)$rotate(c(90, 0, -90), "degrees")
	expect_equal(p3$x, c(-3, 5, 6))
	expect_equal(p3$y, c(2, 4, -7))

	skip_if_not(getRversion() >= "4.3.0")
	expect_equal(rotate2d(90, "degrees"), permute2d("yx") %*% scale2d(-1, 1))
})

test_that("rotate3d()", {
	skip_if_not_installed("withr")
	withr::local_options(affiner_options(default = TRUE))
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	z <- c(5, 2, 3)
	p1 <- as_coord3d(x = x, y = y, z = z)$rotate("z-axis", 90, "degrees")
	expect_equal(p1$x, -y)
	expect_equal(p1$y, x)
	expect_equal(p1$z, z)

	p2 <- as_coord3d(x = x, y = y, z = z)$rotate("x-axis", 90, "degrees")
	expect_equal(p2$x, x)
	expect_equal(p2$y, -z)
	expect_equal(p2$z, y)

	expect_equal(
		rotate3d_to_AA(rotate3d("x-axis", 90, "degrees")),
		list(axis = as_coord3d(1, 0, 0), theta = angle(90, "degrees"))
	)
	expect_equal(
		rotate3d_to_AA(rotate3d("y-axis", 90, "degrees")),
		list(axis = as_coord3d(0, 1, 0), theta = angle(90, "degrees"))
	)
	expect_equal(
		rotate3d_to_AA(rotate3d(as_coord3d(0.5, 0.5, 0.7071068), 180, "degrees")),
		list(axis = as_coord3d(0.5, 0.5, 0.7071068), theta = angle(180, "degrees")),
		tolerance = 1e-6
	)
	expect_equal(
		rotate3d_to_AA(rotate3d(as_coord3d(-0.6, 0.6, 0.8), 180, "degrees")),
		list(axis = as_coord3d(-0.5144958, 0.5144958, 0.6859943), theta = angle(180, "degrees")),
		tolerance = 1e-6
	)
	expect_equal(
		rotate3d_to_AA(rotate3d(as_coord3d(0.6, -0.6, 0.5291503), 180, "degrees")),
		list(axis = as_coord3d(0.6, -0.6, 0.5291503), theta = angle(180, "degrees")),
		tolerance = 1e-6
	)
	expect_equal(
		rotate3d_to_AA(rotate3d(as_coord3d(-0.6, -0.6, 0.5291503), 180, "degrees")),
		list(axis = as_coord3d(-0.6, -0.6, 0.5291503), theta = angle(180, "degrees")),
		tolerance = 1e-6
	)
	expect_equal(
		rotate3d_to_AA(rotate3d(as_coord3d(-0.6, 0.6, 0.5291503), 0, "degrees")),
		list(axis = as_coord3d(0, 0, 1), theta = angle(0, "degrees")),
		tolerance = 1e-6
	)
})

test_that("scale1d()", {
	x <- c(2, 5, 7)
	p1 <- as_coord1d(x = x)$scale(2)
	expect_equal(p1$x, 2 * x)

	p2 <- as_coord1d(x = x)$scale(rep(2, 3))
	expect_equal(p1, p2)
})

test_that("scale2d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	p1 <- as_coord2d(x = x, y = y)$scale(2, 3)
	expect_equal(p1$x, 2 * x)
	expect_equal(p1$y, 3 * y)

	p2 <- as_coord2d(x = x, y = y)$scale(rep(2, 3), rep(3, 3))
	expect_equal(p1, p2)
})

test_that("scale3d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	z <- c(5, 2, 3)
	p1 <- as_coord3d(x = x, y = y, z = z)$scale(2, 3, 4)
	expect_equal(p1$x, 2 * x)
	expect_equal(p1$y, 3 * y)
	expect_equal(p1$z, 4 * z)
	p2 <- as_coord3d(x = x, y = y, z = z)$scale(rep(2, 3), rep(3, 3), rep(4, 3))
	expect_equal(p1, p2)
})

test_that("shear2d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	p1 <- as_coord2d(x = x, y = y)$shear(x = 1)
	expect_equal(p1$x, x + y)
	expect_equal(p1$y, y)

	p2 <- as_coord2d(x = x, y = y)$shear(y = 1)
	expect_equal(p2$x, x)
	expect_equal(p2$y, y + x)

	p3 <- as_coord2d(x = x, y = y)$shear(x = 1, y = 1)
	expect_equal(p3$x, x + y)
	expect_equal(p3$y, y + x)
})

test_that("shear3d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	z <- c(5, 2, 3)
	p1 <- as_coord3d(x = x, y = y, z = z)$shear(xy = 1, xz = 0.5)
	expect_equal(p1$x, x + y + 0.5 * z)
	expect_equal(p1$y, y)
	expect_equal(p1$z, z)

	p2 <- as_coord3d(x = x, y = y, z = z)$shear(zx = 1, yx = 1)
	expect_equal(p2$x, x)
	expect_equal(p2$y, y + x)
	expect_equal(p2$z, z + x)
})

test_that("translate1d()", {
	x <- c(2, 5, 7)
	p0 <- as_coord1d(x = x)

	vec <- as_coord1d(x = 2)
	p1 <- p0$clone()$translate(vec)
	expect_equal(p1$x, x + 2)

	p2 <- p0$clone()$translate("origin")
	expect_equal(p2$x, x)

	p3 <- p0$clone()$translate(p0)
	expect_equal(p3$x, x + x)

	expect_equal(translate1d("origin"), translate1d(as_coord1d(0)))
})

test_that("translate2d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	p0 <- as_coord2d(x = x, y = y)

	vec <- as_coord2d(x = 2, y = 3)
	p1 <- p0$clone()$translate(vec)
	expect_equal(p1$x, x + 2)
	expect_equal(p1$y, y + 3)

	p2 <- p0$clone()$translate(angle(90, "degrees"), radius = 2)
	expect_equal(p2$x, x)
	expect_equal(p2$y, y + 2)

	p3 <- p0$clone()$translate(p0)
	expect_equal(p3$x, x + x)
	expect_equal(p3$y, y + y)

	expect_equal(translate2d(angle(90, "degrees"), radius = 1), translate2d(as_coord2d(0, 1)))
})

test_that("translate3d()", {
	x <- c(2, 5, 7)
	y <- c(3, 4, 6)
	z <- c(5, 2, 3)
	p0 <- as_coord3d(x = x, y = y, z = z)
	expect_equal(p0, p0$clone()$translate("origin"))

	vec <- as_coord3d(x = 2, y = 3, z = 1)
	p1 <- p0$clone()$translate(vec)
	expect_equal(p1$x, x + 2)
	expect_equal(p1$y, y + 3)
	expect_equal(p1$z, z + 1)

	p3 <- p0$clone()$translate(p0)
	expect_equal(p3$x, x + x)
	expect_equal(p3$y, y + y)
	expect_equal(p3$z, z + z)

	expect_equal(translate3d(as_coord3d(3, 2, 1)), translate3d(as_coord2d(3, 2), z = 1))
})
