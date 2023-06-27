test_that("print.line2d()", {
    x <- c(2, 5, 7)
    y <- c(3, 4, 6)
    p1 <- as_coord2d(x = x, y = y)
    p0 <- as_coord2d("origin")

    l1 <- as_line2d(p0, p1)
    l2 <- as_line2d(as_angle(p1), p0)

    expect_equal(l1, l2)
    expect_snapshot(print(l1))

    expect_equal(length(rep_len(l1, 3)), 3L)
    expect_equal(length(rep(l1, 2)), 6L)
    expect_equal(length(c(l1, l2)), 6L)
    expect_equal(is.na(l1), rep(FALSE, 3L))
    expect_equal(is.nan(l1), rep(FALSE, 3L))
    expect_equal(is.infinite(l1), rep(FALSE, 3L))
    expect_equal(is.finite(l1), rep(TRUE, 3L))
})
