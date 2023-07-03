test_that("print.line2d()", {
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

    expect_equal(as_line2d("x-axis"),
                 as_line2d(1, 0, 0))
    expect_equal(as_line2d("y-axis"),
                 as_line2d(0, 1, 0))
    expect_warning(as_line2d("boo"))

    expect_equal(length(rep_len(l1, 3)), 3L)
    expect_equal(length(rep(l1, 2)), 6L)
    expect_equal(length(c(l1, l2)), 6L)
    expect_equal(is.na(l1), rep(FALSE, 3L))
    expect_equal(is.nan(l1), rep(FALSE, 3L))
    expect_equal(is.infinite(l1), rep(FALSE, 3L))
    expect_equal(is.finite(l1), rep(TRUE, 3L))

    expect_true(is_congruent(as_angle(as_line2d(0, 1, 1), "degrees"),
                             as_angle(90, "degrees")))
})
