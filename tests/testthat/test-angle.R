expect_congruent <- function(x, y, ...) expect_true(is_congruent(x, y, ...))

test_that("standardize_angular_unit()", {
	expect_equal(standardize_angular_unit("degrees"), "degrees")
	expect_equal(standardize_angular_unit("degree"), "degrees")
	expect_equal(standardize_angular_unit("deg"), "degrees")

	expect_equal(standardize_angular_unit("half-turn"), "pi-radians")
	expect_equal(standardize_angular_unit("half-revolution"), "pi-radians")
	expect_equal(standardize_angular_unit("pi * radians"), "pi-radians")

	expect_equal(standardize_angular_unit("gradians"), "gradians")
	expect_equal(standardize_angular_unit("gradian"), "gradians")
	expect_equal(standardize_angular_unit("grade"), "gradians")
	expect_equal(standardize_angular_unit("grad"), "gradians")
	expect_equal(standardize_angular_unit("gon"), "gradians")

	expect_equal(standardize_angular_unit("radians"), "radians")
	expect_equal(standardize_angular_unit("radian"), "radians")
	expect_equal(standardize_angular_unit("rad"), "radians")

	expect_equal(standardize_angular_unit("revolution"), "turns")
	expect_equal(standardize_angular_unit("rev"), "turns")
	expect_equal(standardize_angular_unit("turn"), "turns")
	expect_equal(standardize_angular_unit("tr"), "turns")

	expect_error(standardize_angular_unit("foobars"), "Do not recognize angular unit 'foobars'")
})

test_that("format.angle() and print.angle()", {
	skip_if_not(l10n_info()[["UTF-8"]])
	skip_if_not_installed("withr")
	withr::local_options(cli.unicode = TRUE, affiner_angular_unit = "degrees")
	expect_equal(format(angle(180, "degrees")), "180\u00b0")
	expect_equal(format(angle(200, "gradians")), "200 gon")
	expect_equal(format(angle(pi, "radians")), "3.141593 rad")
	expect_equal(format(angle(1, "pi-radians")), "1\u03c0 rad")
	expect_equal(format(angle(0.5, "turns")), "0.5 tr")
	expect_equal(format(angle()), character())

	expect_output(print(angle()), "<angle<degrees>\\[0\\]>")
	expect_output(print(angle(200, "gradians")), "\\[1\\] 200 gon")
})

test_that("to_radians() / from_radians()", {
	expect_equal(from_radians(to_radians(180, "degrees"), "degrees"), 180)
	expect_equal(from_radians(to_radians(1, "pi-radians"), "pi-radians"), 1)
	expect_equal(from_radians(to_radians(100, "gradians"), "gradians"), 100)
	expect_equal(from_radians(to_radians(pi, "radians"), "radians"), pi)
	expect_equal(from_radians(to_radians(0.5, "turns"), "turns"), 0.5)
})

test_that("to_piradians() / from_piradians()", {
	expect_equal(from_piradians(to_piradians(180, "degrees"), "degrees"), 180)
	expect_equal(from_piradians(to_piradians(1, "pi-radians"), "pi-radians"), 1)
	expect_equal(from_piradians(to_piradians(100, "gradians"), "gradians"), 100)
	expect_equal(from_piradians(to_piradians(pi, "radians"), "radians"), pi)
	expect_equal(from_piradians(to_piradians(0.5, "turns"), "turns"), 0.5)
})

test_that("angular_unit()", {
	a <- angle(seq(0, 360, by = 90), "degrees")
	expect_equal(angular_unit(a), "degrees")
	angular_unit(a) <- "degrees"
	angular_unit(a) <- "turns"
	expect_equal(angular_unit(a), "turns")
	expect_equal(as.numeric(a), seq(0, 1, by = 0.25))
	expect_equal(angular_unit(gradians(0)), "gradians")
	expect_equal(angular_unit(turns(0)), "turns")
})

test_that("is_congruent()", {
	a1 <- angle(180, "degrees")
	a2 <- angle(0.5, "turns")
	a3 <- angle(-180, "degrees")
	a4 <- angle(90, "degrees")
	expect_congruent(a1, a2)
	expect_congruent(a1, a3)
	expect_false(is_congruent(a1, a3, mod_turns = FALSE))
	expect_equal(a1, -a3)
	expect_congruent(a1, 180)
	expect_congruent(2 * a1, 360)
	expect_congruent(a1 - a2, 0)
	expect_congruent(a4, as_angle("y-axis", "degrees"))
	expect_congruent(degrees(0), as_angle("x-axis", "degrees"))
	expect_congruent(angle(1e-9, "half-turns"), angle(-1e-9, "half-turns"))
	expect_false(is_congruent(a1, a4))
	expect_congruent(2, 2)
})

test_that("abs()", {
	expect_equal(angle(90, "degrees"), abs(angle(-270, "degrees")))
	expect_equal(angle(100, "gradians"), abs(angle(-300, "gradians")))
	expect_equal(angle(1, "pi-radians"), abs(angle(3, "pi-radians")))
	expect_equal(angle(0.5, "turns"), abs(angle(-0.5, "turns")))
	expect_equal(angle(pi, "radians"), abs(angle(-pi, "radians")))
})

test_that("as.numeric.angle()", {
	a1 <- angle(180, "degrees")
	expect_identical(as.numeric(a1), 180)
	expect_identical(as.numeric(a1, "deg"), 180)
	expect_equal(as.numeric(a1, "turns"), 0.5)
	expect_equal(as.numeric(a1, "radians"), pi)
	a2 <- angle(pi, "radians")
	expect_equal(as.numeric(a2), pi)
	expect_equal(Arg(a2), pi)
	expect_equal(as.numeric(a2, "degrees"), 180)

	c1 <- complex(real = 0, imaginary = 1)
	expect_equal(c1, as.complex(angle(90, "degrees"), modulus = 1))
	expect_congruent(as_angle(c1), angle(90, "degrees"))
})

test_that("base R rounding functions", {
	expect_equal(round(degrees(90.7), 0), degrees(91))
	expect_equal(signif(degrees(184), 2), degrees(180))
	expect_equal(ceiling(degrees(184.2)), degrees(185))
	expect_equal(floor(degrees(184.7)), degrees(184))
	expect_equal(trunc(degrees(184.7)), degrees(184))
})

test_that("base R trigonometric functions", {
	a1 <- angle(180, "degrees")
	expect_equal(cos(a1), cos(pi))
	expect_equal(sin(a1), sin(pi))
	expect_equal(tan(a1), tan(pi))
	a2 <- angle(pi, "radians")
	expect_equal(cos(a2), cos(pi))
	expect_equal(sin(a2), sin(pi))
	expect_equal(tan(a2), tan(pi))
})

test_that("as_angle()", {
	skip_if_not_installed("withr")
	withr::local_options(affiner_options(default = TRUE))
	a1 <- angle(180, "degrees")
	expect_equal(a1, (as_angle(a1)))
	expect_equal(a1, (as_angle(a1, "degrees")))
	expect_equal(a1, degrees(180))
	expect_equal(a1, degrees(angle(1, "pi-radians")))
	expect_congruent(a1, as_angle(a1, "turns"))
	expect_congruent(a1, pi_radians(1))
	expect_congruent(a1, radians(pi))
	expect_false(isTRUE(all.equal(a1, as_angle(a1, "turns"), check.attributes = TRUE)))
	expect_warning(as_angle("foobar"))

	expect_equal(as_angle(as_coord3d("z-axis"), "degrees", type = "azimuth"), angle(0, "degrees"))
	expect_equal(
		as_angle(as_coord3d("z-axis"), "degrees", type = "inclination"),
		angle(0, "degrees")
	)
	expect_equal(as_angle(as_coord3d("x-axis"), "degrees", type = "azimuth"), angle(0, "degrees"))
	expect_equal(
		as_angle(as_coord3d("x-axis"), "degrees", type = "inclination"),
		angle(90, "degrees")
	)
	expect_equal(as_angle(as_coord3d("y-axis"), "degrees", type = "azimuth"), angle(90, "degrees"))
	expect_equal(
		as_angle(as_coord3d("y-axis"), "degrees", type = "inclination"),
		angle(90, "degrees")
	)
	expect_equal(as_angle(as_coord3d(1, 1, -1), "degrees", type = "azimuth"), angle(45, "degrees"))
	expect_equal(
		as_angle(as_coord3d(1, 1, -1), "degrees", type = "inclination"),
		arccosine(-1 / sqrt(3), "degrees")
	)
	expect_equal(
		as_angle(as_coord3d(0, 1, -1), "degrees", type = "inclination"),
		angle(135, "degrees")
	)
})

test_that("c.angle()", {
	a1 <- angle(180, "degrees")
	a2 <- angle(1, "pi-radians")
	expect_equal(c(a1, a2), angle(c(180, 180), "degrees"))

	expect_equal(rep_len(a1, 4), angle(rep(180, 4), "degrees"))
})
