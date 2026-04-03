test_that("makeContent.affine() error when device lacks support", {
	skip_if_not(getRversion() >= "4.2.0")
	local_mocked_bindings(dev_supports_transformations = function() FALSE)
	old_dev <- dev.cur()
	pdf(NULL)
	on.exit(
		{
			dev.off()
			if (old_dev > 1L) dev.set(old_dev)
		},
		add = TRUE
	)
	expect_error(
		grid::grid.draw(affineGrob(grid::nullGrob())),
		"does not support the affine transformation feature"
	)
})

test_that("affineGrob() works", {
	skip_if_not(getRversion() >= "4.3.0")
	skip_if_not(isTRUE(all(capabilities(c("cairo", "png")))))
	old_dev <- dev.cur()
	f <- tempfile(fileext = ".png")
	png(f, type = "cairo")
	on.exit(
		{
			if (old_dev > 1L) {
				dev.set(old_dev)
			}
			unlink(f)
		},
		add = TRUE
	)
	grid.affine(grid::nullGrob())
	dev.off()
	expect_true(file.size(f) > 0)
})
