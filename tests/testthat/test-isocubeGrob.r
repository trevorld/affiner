test_that("isocubeGrob() works", {
	skip_if_not(getRversion() >= "4.3.0")
	skip_if_not(isTRUE(all(capabilities(c("cairo", "png")))))
	f <- tempfile(fileext = ".png")
	png(f, type = "cairo")
	gp_text <- grid::gpar(fontsize = 72)
	grid.isocube(
		top = grid::textGrob("top", gp = gp_text),
		right = grid::textGrob("right", gp = gp_text),
		left = grid::textGrob("left", gp = gp_text)
	)
	dev.off()
	expect_true(file.size(f) > 0)
	unlink(f)
})
