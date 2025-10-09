test_that("`is_parallel()`", {
	line1 <- as_line2d("x-axis")
	line2 <- as_line2d("y-axis")
	line3 <- as_line2d(a = 0, b = 1, c = 2) # y + 2 = 0
	expect_true(is_parallel(line1, line1))
	expect_false(is_parallel(line1, line2))
	expect_false(is_parallel(line1, "y-axis"))
	expect_true(is_parallel(line1, line3))
	expect_false(is_equivalent(line1, line3))

	plane1 <- as_plane3d("xy-plane")
	plane2 <- as_plane3d("xz-plane")
	plane3 <- as_plane3d(a = 0, b = 0, c = 1, d = 2) # z + 2 = 0
	expect_true(is_parallel(plane1, plane1))
	expect_false(is_parallel(plane1, plane2))
	expect_false(is_parallel(plane1, "xz-plane"))
	expect_true(is_parallel(plane1, plane3))
	expect_false(is_equivalent(plane1, plane3))
})
