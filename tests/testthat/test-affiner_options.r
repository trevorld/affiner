test_that("`affiner_options()`", {
	skip_if_not_installed("withr")
	withr::local_options(affiner_options(default = TRUE))
	expect_equal(affiner_options(), affiner_options(default = TRUE))
	expect_true(is.list(affiner_options(affiner_angular_unit = "gradians")))
})
