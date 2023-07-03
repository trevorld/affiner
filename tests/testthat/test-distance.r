test_that("distance2d()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    p2 <- as_coord2d(x = x, y = y)
    p0 <- as_coord2d("origin")
    expect_equal(distance2d(p2, p0),
                 abs(p2))

    l1 <- as_line2d(a = 0, b = 2, c = 4)
    expect_equal(distance2d(l1, p0), 2)
    l2 <- as_line2d(a = 2, b = 0, c = 4)
    expect_equal(distance2d(p0, l2), 2)

    expect_error(distance2d(as_coord3d("origin"), p0))
    expect_error(distance2d(p0, as_coord3d("origin")))
    expect_error(distance2d(l1, as_coord3d("origin")))
})

test_that("distance3d()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    z <- c(2, 7, 3)
    p3 <- as_coord3d(x = x, y = y, z = z)
    p0 <- as_coord3d("origin")

    expect_equal(distance3d(p3, p0),
                 abs(p3))

    pl1 <- as_plane3d(a = 0, b = 2, c = 0, d = 4)
    expect_equal(distance3d(pl1, p0), 2)

    pl2 <- as_plane3d(a = 0, b = 0, c = 2, d = 4)
    expect_equal(distance3d(p0, pl2), 2)

    expect_error(distance3d(pl2, "foobar"))

    expect_error(distance3d(as_coord2d("origin"), p0))
    expect_error(distance3d(p0, as_coord2d("origin")))
})
