test_that("affineGrob() works", {
    skip_if_not(getRversion() >= "4.3.0")
    skip_if_not(isTRUE(capabilities("cairo")))
    f <- tempfile(fileext = ".png")
    png(f, type = "cairo")
    grid.affine(grid::nullGrob())
    dev.off()
    expect_true(file.size(f) > 0)
    unlink(f)
})