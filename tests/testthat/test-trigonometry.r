test_that("trigonometry functions", {
    expect_equal(cosine(angle(0.5, "turns")), -1)

    expect_equal(cosine(200, "gradians"), -1)
    expect_equal(sine(pi, "radians"), 0)
    expect_equal(tangent(1, "pi-radians"), 0)
    expect_equal(secant(90, "degrees"), 1)
    expect_equal(cosecant(180, "degrees"), -1)
    expect_equal(cotangent(0.25, "half-turns"), 1)

    expect_equal(arccosine(-1, "degrees"), angle(180, "degrees"))
    expect_equal(arcsine(0, "turns"), angle(0, "turns"))
    expect_equal(arctangent(0, "gradians"), angle(0, "gradians"))
    expect_equal(arctangent(x=0, y=-1, unit="degrees"), angle(-90, "degrees"))
    expect_equal(arctangent(x=-1, y=0, unit="degrees"), angle(180, "degrees"))
    expect_equal(arccosecant(-1, "degrees"), angle(180, "degrees"))
    expect_equal(arcsecant(1, "degrees"), angle(90, "degrees"))
    expect_equal(arccotangent(1, "half-turns"), angle(0.25, "half-turns"))
})
