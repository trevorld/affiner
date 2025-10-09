test_that("`isotoxal_2ngon_inner_radius()`", {
	expect_equal(isotoxal_2ngon_inner_radius(2, alpha = 90), 1)
	expect_equal(isotoxal_2ngon_inner_radius(2, pi, alpha = 90), pi)
	expect_equal(isotoxal_2ngon_inner_radius(2, beta_ext = 270), 1)
	# |2_60| rhombus has internal angle 60
	scale_2_60 <- 0.57735026918962573106 # `gridpattern::star_scale(2, 60)`
	scale_alpha <- isotoxal_2ngon_inner_radius(2, alpha = 60)
	scale_beta_ext <- isotoxal_2ngon_inner_radius(2, beta_ext = 240)
	expect_equal(scale_2_60, scale_alpha)
	expect_equal(scale_2_60, scale_beta_ext)

	# |8/3| star has outer internal angle 45 degrees
	# and inner external angle 90 degrees
	scale_8_3_star <- 0.54119610014619701222 # `gridpattern::star_scale(8, 45)`
	scale_alpha <- isotoxal_2ngon_inner_radius(8, alpha = 45)
	scale_beta_ext <- isotoxal_2ngon_inner_radius(8, beta_ext = 90)
	scale_d <- isotoxal_2ngon_inner_radius(8, d = 3)
	expect_equal(scale_8_3_star, scale_alpha)
	expect_equal(scale_8_3_star, scale_beta_ext)
	expect_equal(scale_8_3_star, scale_d)
	expect_equal(isotoxal_2ngon_inner_radius(8, pi, d = 3), pi * scale_8_3_star)
})

test_that("`alpha_to_beta_ext()` and `beta_ext_to_alpha()`", {
	expect_equal(alpha_to_beta_ext(2, 90), 270)
	expect_equal(beta_ext_to_alpha(2, 270), 90)
	expect_equal(alpha_to_beta_ext(8, 45), 90)
	expect_equal(beta_ext_to_alpha(8, 90), 45)
	expect_equal(alpha_to_beta_ext(5, 36), 108)
	expect_equal(beta_ext_to_alpha(5, 108), 36)
	expect_equal(alpha_to_beta_ext(6, 60), 120)
	expect_equal(beta_ext_to_alpha(6, 120), 60)
})
