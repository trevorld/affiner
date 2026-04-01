test_that("makeContent.affine() error when device lacks support", {
	skip_if_not(getRversion() >= "4.2.0")
	local_mocked_bindings(dev_supports_transformations = function() FALSE)
	expect_error(
		grid::grid.draw(affineGrob(grid::nullGrob())),
		"does not support the affine transformation feature"
	)
})

test_that("affineGrob() works", {
	skip_if_not(getRversion() >= "4.3.0")
	skip_if_not(isTRUE(all(capabilities(c("cairo", "png")))))
	f <- tempfile(fileext = ".png")
	png(f, type = "cairo")
	grid.affine(grid::nullGrob())
	dev.off()
	expect_true(file.size(f) > 0)
	unlink(f)
})
