test_that("as_line2d()", {
    skip_if_not_installed("withr")
    withr::local_options(affiner_options(default = TRUE))

    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    p1 <- as_coord2d(x = x, y = y)
    p0 <- as_coord2d("origin")

    l1 <- as_line2d(p1 = p0, p2 = as.data.frame(p1))
    l2 <- as_line2d(as_angle(p1), "origin")

    expect_equal(normal2d(as_line2d(1, 1, 0)),
                 as_coord2d(0.5 * sqrt(2), 0.5 * sqrt(2)))

    expect_equal(l1, l2)
    expect_equal(l1, as_line2d(l1))
    expect_snapshot(print(l1))

    expect_equal(length(rep_len(l1, 3)), 3L)
    expect_equal(length(rep(l1, 2)), 6L)
    expect_equal(length(c(l1, l2)), 6L)
    expect_equal(is.na(l1), rep(FALSE, 3L))
    expect_equal(is.nan(l1), rep(FALSE, 3L))
    expect_equal(is.infinite(l1), rep(FALSE, 3L))
    expect_equal(is.finite(l1), rep(TRUE, 3L))

    lxa <- as_line2d("x-axis")
    expect_equal(lxa, as_line2d(0, -1, 0))

    lya <- as_line2d("y-axis")
    expect_equal(lya, as_line2d(1, 0, 0))

    expect_warning(as_line2d("boo"))

    expect_true(is_congruent(as_angle(lxa), degrees(0)))
    expect_true(is_congruent(as_angle(lya), degrees(90)))

    expect_true(is_congruent(2 * as_angle(as_line2d(0, -1, 1), "degrees"),
                             2 * as_angle(0, "degrees")))

    expect_true(is_congruent(2 * as_angle(as_line2d(1, 0, 1), "degrees"),
                             2 * as_angle(90, "degrees")))

    expect_true(is_congruent(2 * as_angle(as_line2d(0, 1, 1), "degrees"),
                             2 * as_angle(180, "degrees")))

    expect_true(is_congruent(2 * as_angle(as_line2d(-1, 0, 1), "degrees"),
                             2 * as_angle(270, "degrees")))

    expect_true(is_congruent(2 * as_angle(as_line2d(-1, 1, 1), "degrees"),
                             2 * as_angle(45, "degrees")))

    expect_true(is_congruent(2 * as_angle(as_line2d(1, 1, 1), "degrees"),
                             2 * as_angle(-45, "degrees")))

    pt <- as_point1d(a = 1, b = -1)
    expect_equal(as_line2d(pt, b = 3),
                 as_line2d(a = 1, b = 3, c = -1))
})
