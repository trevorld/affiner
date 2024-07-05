test_that("as_plane3d()", {
    p1 <- as_plane3d(1, 2, 3, 4)
    expect_true(is_plane3d(p1))

    expect_equal(as_plane3d("xy-plane"),
                 as_plane3d(0, 0, 1, 0))
    expect_equal(p1, as_plane3d(p1))
    expect_warning(as_plane3d("boo"))

    expect_equal(as_plane3d("xy-plane"),
                 as_plane3d(normal3d("xy-plane"), "origin"))

    expect_equal(as_plane3d(as_line2d(1, 0, -2)),
                 as_plane3d(1, 0, 0, -2))

    expect_false(is.na(p1))
    expect_false(is.nan(p1))
    expect_false(is.infinite(p1))
    expect_true(is.finite(p1))
    expect_length(p1, 1L)
    expect_length(rep(p1, 3), 3L)
    expect_length(c(p1, p1), 2L)

    expect_snapshot(print(p1))

    expect_equal(as_plane3d(p1 = as_coord3d("origin"), p2 = "x-axis", p3 = "y-axis"),
                 as_plane3d("xy-plane"))

    pt <- as_point1d(a = 1, b = -1)
    expect_equal(as_plane3d(pt, b = 3, c = 4),
                 as_plane3d(a = 1, b = 3, c = 4, d = -1))
})
