test_that("transform2d()", {
    m <- transform2d(diag(3))
    expect_true(is_transform2d(m))
    expect_equal(m, as_transform2d(m))
    expect_equal(m, as_transform2d(diag(2)))

    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    p1 <- as_coord2d(x = x, y = y)

    expect_equal(p1, p1$transform()$transform(diag(3)))
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

test_that("project2d()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    p1 <- as_coord2d(x = x, y = y)$project(as_line2d("x-axis"))
    expect_equal(p1$x, x)
    expect_equal(p1$y, rep(0, 3))

    expect_equal(as.double(as_coord2d(x, y), line = "x-axis"), p1$x)

    p2 <- as_coord2d(x = x, y = y)$project(degrees(90))
    expect_equal(p2$x, rep(0, 3))
    expect_equal(p2$y, y)

    p3 <- as_coord2d(x = x, y = y)$project(scale = 0.5)
    expect_equal(p3$x, x + 0.5 * y)
    expect_equal(p3$y, rep(0, 3))
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
})

test_that("reflect2d()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    p1 <- as_coord2d(x = x, y = y)$reflect("x-axis")
    expect_equal(p1$x, x)
    expect_equal(p1$y, -y)
    p2 <- as_coord2d(x = x, y = y)$reflect("y-axis")
    expect_equal(p2$x, -x)
    expect_equal(p2$y, y)
    p3 <- as_coord2d(x = x, y = y)$reflect(degrees(135))
    expect_equal(p3$x, -y)
    expect_equal(p3$y, -x)
    p4 <- as_coord2d(x = x, y = y)$reflect(as_coord2d(1, 1))
    expect_equal(p4$x, -y)
    expect_equal(p4$y, -x)
})

test_that("reflect3d()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    z <- c(4, 9, 3)
    p1 <- as_coord3d(x = x, y = y, z = z)$reflect("xy-plane")
    expect_equal(p1$x, x)
    expect_equal(p1$y, y)
    expect_equal(p1$z, -z)

    p2 <- as_coord3d(x = x, y = y, z = z)$reflect("xz-plane")
    expect_equal(p2$x, x)
    expect_equal(p2$y, -y)
    expect_equal(p2$z, z)

    p3 <- as_coord3d(x = x, y = y, z = z)$reflect("yz-plane")
    expect_equal(p3$x, -x)
    expect_equal(p3$y, y)
    expect_equal(p3$z, z)

    p4 <- as_coord3d(x = x, y = y, z = z)$reflect(as_coord3d(1, 0, 1))
    expect_equal(p4$x, -z)
    expect_equal(p4$y, y)
    expect_equal(p4$z, -x)
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
    expect_equal(rotate2d(90, "degrees"),
                 permute2d("yx") %*% scale2d(-1, 1))
})

test_that("rotate3d()", {
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

    expect_equal(rotate3d_to_AA(rotate3d("x-axis", 90, "degrees")),
                 list(axis = as_coord3d(1, 0, 0), theta = angle(90, "degrees")))
    expect_equal(rotate3d_to_AA(rotate3d("y-axis", 90, "degrees")),
                 list(axis = as_coord3d(0, 1, 0), theta = angle(90, "degrees")))
    expect_equal(rotate3d_to_AA(rotate3d(as_coord3d(0.5, 0.5, 0.7071068), 180, "degrees")),
                 list(axis = as_coord3d(0.5, 0.5, 0.7071068), theta = angle(180, "degrees")),
                 tolerance = 1e-6)
    expect_equal(rotate3d_to_AA(rotate3d(as_coord3d(-0.6, 0.6, 0.8), 180, "degrees")),
                 list(axis = as_coord3d(-0.5144958, 0.5144958, 0.6859943), theta = angle(180, "degrees")),
                 tolerance = 1e-6)
    expect_equal(rotate3d_to_AA(rotate3d(as_coord3d(0.6, -0.6, 0.5291503), 180, "degrees")),
                 list(axis = as_coord3d(0.6, -0.6, 0.5291503), theta = angle(180, "degrees")),
                 tolerance = 1e-6)
    expect_equal(rotate3d_to_AA(rotate3d(as_coord3d(-0.6, -0.6, 0.5291503), 180, "degrees")),
                 list(axis = as_coord3d(-0.6, -0.6, 0.5291503), theta = angle(180, "degrees")),
                 tolerance = 1e-6)
    expect_equal(rotate3d_to_AA(rotate3d(as_coord3d(-0.6, 0.6, 0.5291503), 0, "degrees")),
                 list(axis = as_coord3d(0, 0, 1), theta = angle(0, "degrees")),
                 tolerance = 1e-6)
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

    expect_equal(translate2d(angle(90, "degrees"), radius = 1),
                 translate2d(as_coord2d(0, 1)))
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

    expect_equal(translate3d(as_coord3d(3, 2, 1)),
                 translate3d(as_coord2d(3, 2), z = 1))
})
