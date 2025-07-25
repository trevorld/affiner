test_that("as_point1d()", {
    x <- c(2, 5, 7)
    p1 <- as_point1d(as_coord1d(x = x))
    p0a <- as_point1d("origin")
    p0b <- as_point1d("zero")
    p0c <- as_point1d(as_coord1d("origin"))

    expect_equal(p0a, p0b)
    expect_equal(p0a, p0c)
    expect_equal(p1, as_point1d(p1))
    expect_equal(p1, p1[1:3])
    expect_snapshot(print(p1))

    expect_warning(as_point1d("boo"))

    expect_equal(length(rep_len(p1, 3)), 3L)
    expect_equal(length(rep(p1, 2)), 6L)
    expect_equal(length(c(p1, p1)), 6L)
    expect_equal(is.na(p1), rep(FALSE, 3L))
    expect_equal(is.nan(p1), rep(FALSE, 3L))
    expect_equal(is.infinite(p1), rep(FALSE, 3L))
    expect_equal(is.finite(p1), rep(TRUE, 3L))
    expect_true(is.data.frame(as.data.frame(p1)))
    expect_true(is.list(as.list(p1)))
})

test_that("`is_equivalent.Point1D()`", {
    x <- c(2, 5, 7)
    c1 <- as_coord1d(x = x)
    p1 <- as_point1d(c1)
    expect_equal(is_equivalent(p1, as_point1d(a = 2, b = -10)),
                 c(FALSE, TRUE, FALSE))
    expect_equal(is_equivalent(as_point1d(a = 2, b = -10), p1),
                 c(FALSE, TRUE, FALSE))
    expect_equal(is_equivalent(p1, c1), rep(TRUE, 3L))
})
