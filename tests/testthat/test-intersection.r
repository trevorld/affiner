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
    expect_equal(has_intersection(line, coord),
                 c(TRUE, FALSE, FALSE))
    expect_equal(has_intersection(coord, line),
                 c(TRUE, FALSE, FALSE))

    plane <- as_plane3d("xy-plane")
    coord <- as_coord3d(x = 0, y = 0, z = 0:2)
    expect_equal(has_intersection(plane, coord),
                 c(TRUE, FALSE, FALSE))
    expect_equal(has_intersection(coord, plane),
                 c(TRUE, FALSE, FALSE))
})

test_that("`intersection()`", {
    line1 <- as_line2d("x-axis")
    line2 <- as_line2d("y-axis")
    line3 <- as_line2d(a = 0, b = 1, c = 2) # y + 2 = 0
    line4 <- as_line2d(a = 1, b = 0, c = 2) # x + 2 = 0
    expect_equal(intersection(line1, line1),
                 list(standardize(line1)))
    expect_equal(intersection(line1, line2),
                 list(as_coord2d("origin")))
    expect_equal(intersection(line1, "y-axis"),
                 list(as_coord2d("origin")))
    expect_equal(intersection(line1, line3),
                 list(NULL))
    expect_equal(intersection(line3, line4),
                 list(as_coord2d(x = -2, y = -2)))

    plane1 <- as_plane3d("xy-plane")
    plane2 <- as_plane3d("xz-plane")
    plane3 <- as_plane3d(a = 0, b = 0, c = 1, d = 2) # z + 2 = 0
    expect_equal(intersection(plane1, plane1),
                 list(standardize(plane1)))
    expect_error(intersection(plane1, plane2))
    expect_error(intersection(plane1, "xz-plane"))
    expect_equal(intersection(plane1, plane3),
                 list(NULL))

    point1 <- as_point1d("origin")
    point2 <- as_point1d(a = 1, b = 2)
    expect_equal(intersection(point1, "origin"),
                 list(standardize(point1)))
    expect_equal(intersection(point1, point2),
                 list(NULL))


    p1 <- as_coord1d("origin")
    p2 <- as_coord1d(x = 3)
    expect_equal(intersection(p1, "origin"),
                 list(p1))
    expect_equal(intersection(p1, p2),
                 list(NULL))

    p1 <- as_coord2d("origin")
    p2 <- as_coord2d(x = 3)
    expect_equal(intersection(p1, "origin"),
                 list(p1))
    expect_equal(intersection(p1, p2),
                 list(NULL))

    p1 <- as_coord3d("origin")
    p2 <- as_coord3d(x = 3)
    expect_equal(intersection(p1, "origin"),
                 list(p1))
    expect_equal(intersection(p1, p2),
                 list(NULL))
})
