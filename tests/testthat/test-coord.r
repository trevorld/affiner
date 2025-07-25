test_that("coord1d()", {
    x <- c(2, 5, 7)
    p1 <- as_coord1d(x = x)
    expect_true(is_coord1d(p1))
    expect_equal(p1$x, x)
    expect_length(p1, 3)

    expect_equal(p1[2:3], as_coord1d(x = x[2:3]))

    p2 <- as_coord1d("origin")
    expect_equal(rep_len(p2, 4), as_coord1d(rep(0, 4)))
    expect_true(is_equivalent(p2, "origin"))
})

test_that("coord2d()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    p1 <- as_coord2d(x = x, y = y)
    expect_true(is_coord2d(p1))
    expect_equal(p1$x, x)
    expect_equal(p1$y, y)
    expect_length(p1, 3)

    expect_equal(p1[2:3], as_coord2d(x = x[2:3], y = y[2:3]))

    p2 <- as_coord2d("origin")
    expect_equal(rep_len(p2, 4), as_coord2d(rep(0, 4), rep(0, 4)))
    expect_true(is_equivalent(p2, "origin"))
})

test_that("coord3d()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    z <- c(2, 7, 3)
    p1 <- as_coord3d(x = x, y = y, z = z)

    expect_true(is_coord3d(p1))
    expect_equal(p1$x, x)
    expect_equal(p1$y, y)
    expect_equal(p1$z, z)
    expect_length(p1, 3)

    expect_equal(p1[2:3], as_coord3d(x = x[2:3], y = y[2:3], z = z[2:3]))

    p3 <- as_coord3d("origin")
    expect_equal(rep_len(p3, 4), as_coord3d(rep(0, 4), rep(0, 4), rep(0, 4)))
    expect_true(is_equivalent(p3, "origin"))
})

test_that("convex_hull2d()", {
    p <- as_coord2d(x = rnorm(25), y = rnorm(25))
    expect_equal(convex_hull2d(p),
                 p[rev(grDevices::chull(as.list(p)))])
})

test_that("mean()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    z <- c(2, 7, 3)
    p1 <- as_coord1d(x = x)
    expect_equal(mean(p1), as_coord1d(mean(x)))
    expect_equal(mean(p1), sum(p1, na.rm = TRUE) / length(p1))

    p2 <- as_coord2d(x = x, y = y)
    expect_equal(mean(p2), as_coord2d(mean(x), mean(y)))
    expect_equal(mean(p2), sum(p2, na.rm = TRUE) / length(p2))

    p3 <- as_coord3d(x = x, y = y, z = z)
    expect_equal(mean(p3), as_coord3d(mean(x), mean(y), mean(z)))
    expect_equal(mean(p3), sum(p3, na.rm = TRUE) / length(p3))
})

test_that("as_coord1d()", {
    expect_warning(as_coord1d("foobar"))

    l <- list(x = 1:10)
    p <- as_coord1d(l)
    expect_equal(p$x, l$x)

    m <- as.matrix(1:10)
    p <- as_coord1d(m)
    expect_equal(p$x, 1:10)
    expect_true(is.data.frame(as.data.frame(p)))
    expect_true(is.list(as.list(p)))

    expect_equal(as_coord1d(p), p)
    expect_equal(as_coord1d(as.data.frame(p)), p)

    # Projections form Coord2D
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    p1 <- as_coord2d(x = x, y = y)
    expect_equal(as_coord1d(p1, line = "x-axis")$x, x)
    expect_equal(as_coord1d(p1, scale = 0.5)$x, x + 0.5 * y)
    expect_equal(as_coord1d(p1, permutation = "yx")$x, y)
    expect_equal(as_coord1d(p1, permutation = "yx", scale = 0.5)$x, y + 0.5 * x)

    line <- as_line2d(a = 0, b = -1, c = 2)
    expect_equal(as_coord1d(p1, line = line)$x, x)
    expect_equal(as_coord1d(p1, line = line, scale = 0.5)$x, x + 0.5 * (y - 2))

    line <- as_line2d(a = 1, b = 0, c = -2)
    expect_equal(as_coord1d(p1, line = line)$x, y)

    x6 <- c(-5, 5, 5)
    y6 <- c(5, -5, 5)
    l6 <- as_line2d(a = 1, b = -1, c = 0)
    p6 <- as_coord2d(x = x6, y = y6)
    expect_equal(as_coord1d(p6, line = l6)$x, c(0, 0, sqrt(50)))


    l7 <- as_line2d(a = 1, b = -1, c = 2)
    p7 <- as_coord2d(x = x6, y = y6)
    expect_equal(as_coord1d(p7, line = l7)$x, c(0, 0, sqrt(50)))
})

test_that("as_coord2d()", {
    v1 <- as_coord2d(angle(90, "degrees"), radius = 2)
    expect_equal(v1$x, 0)
    expect_equal(v1$y, 2)

    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    p1 <- as_coord2d(x = x, y = y)
    expect_equal(p1 == p1, rep_len(TRUE, 3))
    expect_equal(p1 != p1, rep_len(FALSE, 3))

    expect_equal(p1, as_coord2d(as.data.frame(p1)))
    expect_true(is.matrix(as.matrix(p1)))
    expect_true(is.data.frame(as.data.frame(p1)))
    expect_true(is.list(as.list(p1)))

    expect_equal(p1, as_coord2d(p1))


    l <- list(x = x, y = y)
    expect_equal(p1, as_coord2d(l))
    m <- as.matrix(as.data.frame(l))
    expect_equal(p1, as_coord2d(m))

    expect_equal(as_coord2d("origin"), as_coord2d(0, 0))
    expect_equal(as_coord2d("x-axis"), as_coord2d(1, 0))
    expect_equal(as_coord2d("y-axis"), as_coord2d(0, 1))
    expect_warning(as_coord2d("foobar"))

    # Projections from Coord3D
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    z <- c(4, 9, 3)
    p <- as_coord3d(x, y, z)
    p1 <- as_coord2d(p)
    expect_equal(p1$x, x)
    expect_equal(p1$y, y)

    p2 <- as_coord2d(p, "xzy")
    expect_equal(p2$x, x)
    expect_equal(p2$y, z)

    p3 <- as_coord2d(p, "xzy",
                     plane = "xy-plane",
                     scale = 0.5,
                     alpha = 60, unit = "degrees")
    scale <- 0.5
    alpha <- angle(60, "degrees")
    expect_equal(p3$x, x + scale * cos(alpha) * y)
    expect_equal(p3$y, z + scale * sin(alpha) * y)

    # projection to plane parallel to xy-plane
    plane4 <- as_plane3d(a = 0, b = 0, c = 1, d = -3)
    p4 <- as_coord2d(p, "xzy",
                     plane = plane4,
                     scale = 0.5,
                     alpha = 60, unit = "degrees")
    expect_equal(p4$x, x + scale * cos(alpha) * (y - 3))
    expect_equal(p4$y, z + scale * sin(alpha) * (y - 3))

    p5 <- as_coord2d(p, "xzy", plane = plane4)
    expect_equal(p5$x, x)
    expect_equal(p5$y, z)

    p6 <- as_coord2d(p, plane = plane4)
    expect_equal(p6$x, x)
    expect_equal(p6$y, y)

    # orthographic projections to axis-aligned planes through origin
    p_xy1 <- as_coord2d(p, plane = "xy-plane")
    p_xy2 <- as_coord2d(p, permutation = "xyz")
    expect_equal(p_xy1$x, x)
    expect_equal(p_xy1$y, y)
    expect_equal(p_xy1, p_xy2)

    p_yx1 <- as_coord2d(p, plane = "yx-plane")$rotate(degrees(-90))
    p_yx2 <- as_coord2d(p, permutation = "yxz")
    expect_equal(p_yx1$x, y)
    expect_equal(p_yx1$y, x)
    expect_equal(p_yx1, p_yx2)

    p_xz1 <- as_coord2d(p, plane = "xz-plane")$rotate(degrees(-90))
    p_xz2 <- as_coord2d(p, permutation = "xzy")
    expect_equal(p_xz1$x, x)
    expect_equal(p_xz1$y, z)
    expect_equal(p_xz1, p_xz2)

    p_zx1 <- as_coord2d(p, plane = "zx-plane")$rotate(degrees(180))
    p_zx2 <- as_coord2d(p, permutation = "zxy")
    expect_equal(p_zx1$x, z)
    expect_equal(p_zx1$y, x)
    expect_equal(p_zx1, p_zx2)

    p_yz1 <- as_coord2d(p, plane = "yz-plane")$rotate(degrees(-90))
    p_yz2 <- as_coord2d(p, permutation = "yzx")
    expect_equal(p_yz1$x, y)
    expect_equal(p_yz1$y, z)
    expect_equal(p_yz1, p_yz2)

    p_zy1 <- as_coord2d(p, plane = "zy-plane")$rotate(degrees(180))
    p_zy2 <- as_coord2d(p, permutation = "zyx")
    expect_equal(p_zy1$x, z)
    expect_equal(p_zy1$y, y)
    expect_equal(p_zy1, p_zy2)
})

test_that("as_coord3d()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    z <- c(2, 7, 3)
    p1 <- as_coord3d(x = x, y = y, z = z)

    expect_equal(p1, as_coord3d(as.data.frame(p1)))
    expect_equal(p1 == p1, rep_len(TRUE, 3))
    expect_equal(p1 != p1, rep_len(FALSE, 3))
    expect_true(is.matrix(as.matrix(p1)))
    expect_true(is.data.frame(as.data.frame(p1)))
    expect_true(is.list(as.list(p1)))

    expect_equal(p1, as_coord3d(p1))

    l <- list(x = x, y = y, z = z)
    expect_equal(p1, as_coord3d(l))
    m <- as.matrix(as.data.frame(l))
    expect_equal(p1, as_coord3d(m))
    l <- list(x = x, y = y)
    expect_equal(as_coord3d(x, y, 0), as_coord3d(l))
    expect_equal(as_coord3d(x, y, z), as_coord3d(l, z = z))

    expect_equal(as_coord3d("origin"), as_coord3d(0, 0, 0))
    expect_equal(as_coord3d("x-axis"), as_coord3d(1, 0, 0))
    expect_equal(normal3d("yz-plane"), as_coord3d(1, 0, 0))
    expect_equal(normal3d("zy-plane"), as_coord3d(-1, 0, 0))
    expect_equal(as_coord3d("y-axis"), as_coord3d(0, 1, 0))
    expect_equal(normal3d("xz-plane"), as_coord3d(0, -1, 0))
    expect_equal(normal3d("zx-plane"), as_coord3d(0, 1, 0))
    expect_equal(as_coord3d("z-axis"), as_coord3d(0, 0, 1))
    expect_equal(normal3d("xy-plane"), as_coord3d(0, 0, 1))
    expect_equal(normal3d("yx-plane"), as_coord3d(0, 0, -1))
    expect_warning(as_coord3d("foobar"))

    # spherical coordinates
    expect_equal(as_coord3d(angle(1 / 3, "pi-radians"), radius = 8,
                            inclination = 1 / 6, unit = "pi-radians"),
                 as_coord3d(2, 2 * sqrt(3), 4 * sqrt(3)))
    # cylindrical coordinates
    expect_equal(as_coord3d(arctangent(-3), radius = sqrt(10), z = "5"),
                 as_coord3d(1, -3, 5))
})

test_that("complex", {
    c1 <- complex(real = 1:4, imaginary = 1:4)
    p1 <- as_coord2d(c1)
    expect_equal(c1, as.complex(p1))
    expect_equal(Re(c1), Re(p1))
    expect_equal(Im(c1), Im(p1))
    expect_equal(Mod(c1), Mod(p1))
    expect_equal(Arg(c1), Arg(p1))
    expect_equal(Conj(c1), Conj(p1))
    expect_equal(abs(c1), abs(p1))
    expect_equal(abs(p1), sqrt(p1 * p1))

    p2 <- as_coord3d(p1, z = 0)
    expect_equal(abs(p1), abs(p2))
    expect_equal(abs(p2), sqrt(p2 * p2))
})

test_that("Ops", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    z <- c(2, 7, 3)
    p1 <- as_coord1d(x = x)
    p2 <- as_coord2d(x = x, y = y)
    p3 <- as_coord3d(x = x, y = y, z = z)

    expect_true(all(p1 == p1))
    expect_true(all(p2 == p2))
    expect_true(all(p3 == p3))

    expect_false(any(p1 != p1))
    expect_false(any(p2 != p2))
    expect_false(any(p3 != p3))

    expect_equal(p1, +p1)
    expect_equal(p2, +p2)
    expect_equal(p3, +p3)

    expect_equal(p1 + as_coord1d(1), as_coord1d(x + 1))
    expect_equal(as_coord1d(1) + p1, as_coord1d(x + 1))
    expect_equal(p2 + as_coord2d(1, 1), as_coord2d(x + 1, y + 1))
    expect_equal(as_coord2d(1, 1) + p2, as_coord2d(x + 1, y + 1))
    expect_equal(p3 + as_coord3d(1, 1, 1), as_coord3d(x + 1, y + 1, z + 1))
    expect_equal(as_coord3d(1, 1, 1) + p3, as_coord3d(x + 1, y + 1, z + 1))

    expect_equal(p1$clone()$scale(-1), -p1)
    expect_equal(p2$clone()$scale(-1), -p2)
    expect_equal(p3$clone()$scale(-1), -p3)

    expect_equal(p1 - as_coord1d(1), as_coord1d(x - 1))
    expect_error(as_coord1d(1) - p1)
    expect_equal(p2 - as_coord2d(1, 1), as_coord2d(x - 1, y - 1))
    expect_error(as_coord2d(1, 1) - p2)
    expect_equal(p3 - as_coord3d(1, 1, 1), as_coord3d(x - 1, y - 1, z - 1))
    expect_error(as_coord3d(1, 1, 1) - p3)

    expect_equal(p1 * p1, x * x)

    expect_equal(p1, 1 * p1)
    expect_equal(p2, 1 * p2)
    expect_equal(p3, 1 * p3)

    expect_equal(p1, p1 * 1)
    expect_equal(p2, p2 * 1)
    expect_equal(p3, p3 * 1)

    expect_error(p1 * "foobar")
    expect_error(p2 * "foobar")
    expect_error(p3 * "foobar")

    expect_error(p1 / "foobar")
    expect_error(p2 / "foobar")
    expect_error(p3 / "foobar")
})

test_that("rounding S3 generics", {
    expect_equal(as_coord1d(1),
                 floor(as_coord1d(1.2)))
    expect_equal(as_coord2d(1, 1),
                 floor(as_coord2d(1.2, 1.8)))
    expect_equal(as_coord3d(1, 1, 3),
                 floor(as_coord3d(1.2, 1.8, 3.2)))

    expect_equal(as_coord1d(2),
                 ceiling(as_coord1d(1.2)))
    expect_equal(as_coord2d(2, 2),
                 ceiling(as_coord2d(1.2, 1.8)))
    expect_equal(as_coord3d(2, 2, 4),
                 ceiling(as_coord3d(1.2, 1.8, 3.2)))

    expect_equal(as_coord1d(1),
                 round(as_coord1d(1.2)))
    expect_equal(as_coord2d(1, 2),
                 round(as_coord2d(1.2, 1.8)))
    expect_equal(as_coord3d(1, 2, 3),
                 round(as_coord3d(1.2, 1.8, 3.2)))

    expect_equal(as_coord1d(1.2),
                 signif(as_coord1d(1.2)))
    expect_equal(as_coord2d(1.2, 1.8),
                 signif(as_coord2d(1.2, 1.8)))
    expect_equal(as_coord3d(1.2, 1.8, 3.2),
                 signif(as_coord3d(1.2, 1.8, 3.2)))

    expect_equal(as_coord1d(1),
                 trunc(as_coord1d(1.2)))
    expect_equal(as_coord2d(1, 1),
                 trunc(as_coord2d(1.2, 1.8)))
    expect_equal(as_coord3d(1, 1, 3),
                 trunc(as_coord3d(1.2, 1.8, 3.2)))
})

test_that("`c.Coord1D()`, `c.Coord2D()`, and `c.Coord3D()`", {
    p1 <- as_coord1d(1:4)
    p2 <- as_coord1d(5:8)
    expect_equal(c(p1, p2), as_coord1d(1:8))

    p1 <- as_coord2d(1:4, 1:4)
    p2 <- as_coord2d(5:8, 5:8)
    expect_equal(c(p1, p2), as_coord2d(1:8, 1:8))

    p1 <- as_coord3d(1:4, 1:4, 1:4)
    p2 <- as_coord3d(5:8, 5:8, 5:8)
    expect_equal(c(p1, p2), as_coord3d(1:8, 1:8, 1:8))
})

test_that("print.Coord1D()", {
    x <- c(2, 5, 7)
    p1 <- as_coord1d(x = x)
    expect_snapshot(print(p1))
    expect_snapshot(print(as_coord1d(numeric(0))))
})

test_that("print.Coord2D()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    p1 <- as_coord2d(x = x, y = y)
    expect_snapshot(print(p1))
    expect_snapshot(print(as_coord2d(numeric(0))))
})

test_that("print.Coord3D()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    z <- c(2, 7, 3)
    p1 <- as_coord3d(x = x, y = y)
    expect_snapshot(print(p1))
    expect_snapshot(print(as_coord3d(numeric(0))))
})

# nolint start
# test_that("scalar projections", {
#     x <- c(2, 5, 7)
#     y <- c(3, 4, 6)
#     z <- c(4, 9, 3)
#     p1 <- as_coord2d(x = x, y = y)
#     expect_equal(as.double(p1), x)
#
#     p3 <- as_coord3d(x = x, y = y, z = z)
#     expect_equal(as.double(p3), x)
#
#     y <- as_coord2d(1, -1)
#     n <- normal2d(as_coord2d(0.5, 0.5))
#     expect_equal(n * p1,
#                  as.double(p1, y))
#
#     p2 <- as_coord2d(2, 1)
#     n <- as_coord2d(0, -1)
#     expect_equal(n * p2,
#                  as.double(p2, n))
# })
# nolint end

test_that("cross_product3d", {
    x <- as_coord3d(2, 3, 4)
    y <- as_coord3d(5, 6, 7)
    expect_equal(cross_product3d(x, y), as_coord3d(-3, 6, -3))

    skip_if_not(getRversion() >= "4.4.0")
    expect_equal(crossprod(x, y), as_coord3d(-3, 6, -3))
})

test_that("normal2d", {
    expect_equal(normal2d(as_coord2d(x = 5, y = 3), normalize = FALSE),
                 as_coord2d(3, -5))
    expect_equal(normal2d(as_line2d(angle(0, "degrees"))),
                 as_coord2d(0, -1))
})

test_that("normal3d", {
    expect_equal(normal3d("xy-plane"),
                 as_coord3d(0, 0, 1))
    expect_equal(normal3d(as_coord3d("x-axis"),
                          cross = "y-axis"),
                 as_coord3d(0, 0, 1))
    expect_equal(normal3d(as_coord3d(2, 0, 0),
                          cross = as_coord3d(0, 2, 0)),
                 as_coord3d(0, 0, 1))
    expect_warning(normal3d("foobar"))
})

test_that("is functions", {
    p <- as_coord1d(x = c(2, NaN, Inf, NA_real_))
    expect_equal(is.na(p), c(FALSE, TRUE, FALSE, TRUE))
    expect_equal(is.nan(p), c(FALSE, TRUE, FALSE, FALSE))
    expect_equal(is.infinite(p), c(FALSE, FALSE, TRUE, FALSE))
    expect_equal(is.finite(p), c(TRUE, FALSE, FALSE, FALSE))
    p <- as_coord2d(x = c(2, NaN, Inf, NA_real_), y = 0)
    expect_equal(is.na(p), c(FALSE, TRUE, FALSE, TRUE))
    expect_equal(is.nan(p), c(FALSE, TRUE, FALSE, FALSE))
    expect_equal(is.infinite(p), c(FALSE, FALSE, TRUE, FALSE))
    expect_equal(is.finite(p), c(TRUE, FALSE, FALSE, FALSE))
    p <- as_coord3d(x = c(2, NaN, Inf, NA_real_), y = 0, z = 0)
    expect_equal(is.na(p), c(FALSE, TRUE, FALSE, TRUE))
    expect_equal(is.nan(p), c(FALSE, TRUE, FALSE, FALSE))
    expect_equal(is.infinite(p), c(FALSE, FALSE, TRUE, FALSE))
    expect_equal(is.finite(p), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("range()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    z <- c(2, 7, 3)
    expect_equal(range(as_coord1d(x), na.rm = TRUE),
                 as_coord1d(c(2, 7)))
    expect_equal(range(as_coord2d(x, y), na.rm = TRUE),
                 as_coord2d(c(2, 7), c(3, 6)))
    expect_equal(range(as_coord3d(x, y, z), na.rm = TRUE),
                 as_coord3d(c(2, 7), c(3, 6), c(2, 7)))
})
