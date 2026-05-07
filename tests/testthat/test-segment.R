p1 <- as_coord2d(x = c(0, 2), y = c(0, 0))
p2 <- as_coord2d(x = c(2, 2), y = c(0, 2))
s <- as_segment2d(p1, p2 = p2)

test_that("as_segment2d() constructs a Segment2D", {
	expect_r6_class(s, "Segment2D")
	expect_r6_class(s, "Coord2D")
	expect_snapshot(print(s))
})

test_that("is_segment2d() works", {
	expect_true(is_segment2d(s))
	expect_false(is_segment2d(p1))
})

test_that("Segment2D$p1 returns first endpoints", {
	expect_r6_class(s$p1, "Coord2D")
	expect_equal(s$p1$x, c(0, 2))
	expect_equal(s$p1$y, c(0, 0))
})

test_that("Segment2D$p2 returns second endpoints", {
	expect_r6_class(s$p2, "Coord2D")
	expect_equal(s$p2$x, c(2, 2))
	expect_equal(s$p2$y, c(0, 2))
})

test_that("Segment2D$mid_point returns midpoints and caches", {
	m <- s$mid_point
	expect_r6_class(m, "Coord2D")
	expect_equal(m$x, c(1, 2))
	expect_equal(m$y, c(0, 1))
	expect_identical(s$mid_point, m)
})

test_that("length() returns number of segments", {
	expect_equal(length(s), 2L)
})

test_that("[.Segment2D subsets correctly", {
	s1 <- s[1]
	expect_r6_class(s1, "Segment2D")
	expect_equal(length(s1), 1L)
	expect_equal(s1$p1$x, 0)
	expect_equal(s1$p2$x, 2)
})

test_that("rep.Segment2D works", {
	sr <- rep(s, 2L)
	expect_equal(length(sr), 4L)
	expect_equal(sr$p1$x, c(0, 2, 0, 2))
})

test_that("as_segment2d() vec argument works", {
	vec <- as_coord2d(x = c(2, 0), y = c(0, 2))
	sv <- as_segment2d(p1, vec = vec)
	expect_equal(sv$p2$x, c(2, 2))
	expect_equal(sv$p2$y, c(0, 2))
})

test_that("as_segment2d.Polygon2D() returns edges", {
	poly <- as_polygon2d(as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)))
	expect_r6_class(as_segment2d(poly), "Segment2D")
	expect_equal(length(as_segment2d(poly)), 4L)
})

test_that("as_segment2d.default() errors", {
	expect_error(as_segment2d(1))
})

test_that("transformations update p1, p2, and mid_point; cache is cleared", {
	s2 <- as_segment2d(p1, p2 = p2)
	s2$translate(as_coord2d(1, 1))
	expect_equal(s2$p1$x, c(1, 3))
	expect_equal(s2$p1$y, c(1, 1))
	expect_equal(s2$p2$x, c(3, 3))
	expect_equal(s2$p2$y, c(1, 3))
	expect_equal(s2$mid_point$x, c(2, 3))
	expect_equal(s2$mid_point$y, c(1, 2))
})

test_that("rotation updates p2 and mid_point correctly", {
	s2 <- as_segment2d(
		as_coord2d(0, 0),
		p2 = as_coord2d(1, 0)
	)
	s2$rotate(degrees(90))
	expect_equal(s2$p1$x, 0, tolerance = 1e-9)
	expect_equal(s2$p1$y, 0, tolerance = 1e-9)
	expect_equal(s2$p2$x, 0, tolerance = 1e-9)
	expect_equal(s2$p2$y, 1, tolerance = 1e-9)
	expect_equal(s2$mid_point$x, 0, tolerance = 1e-9)
	expect_equal(s2$mid_point$y, 0.5, tolerance = 1e-9)
})

test_that("Polygon2D$edges returns a Segment2D of n edges", {
	poly <- as_polygon2d(as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)))
	e <- poly$edges
	expect_r6_class(e, "Segment2D")
	expect_equal(length(e), 4L)
	# first edge: (0,0) -> (1,0)
	expect_equal(e$p1$x[1], 0)
	expect_equal(e$p1$y[1], 0)
	expect_equal(e$p2$x[1], 1)
	expect_equal(e$p2$y[1], 0)
	# edges are cached
	expect_identical(poly$edges, e)
})

test_that("Polygon2D$edges cache is invalidated by transformation", {
	poly <- as_polygon2d(as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)))
	e1 <- poly$edges
	poly$translate(as_coord2d(1, 0))
	e2 <- poly$edges
	expect_equal(e2$p1$x[1], 1)
	expect_false(identical(e1, e2))
})

test_that("sort.Segment2D() orders farthest first by default", {
	p1 <- as_coord2d(x = c(0, 2, 1), y = c(0, 0, 2))
	p2 <- as_coord2d(x = c(2, 1, 0), y = c(0, 2, 0))
	s <- as_segment2d(p1, p2 = p2)
	s_sorted <- sort(s, alpha = degrees(45))
	depths <- painter_depth(s, scale = 1, alpha = degrees(45))
	expect_r6_class(s_sorted, "Segment2D")
	expect_equal(length(s_sorted), length(s))
	expect_equal(painter_depth(s_sorted, scale = 1, alpha = degrees(45)), sort(depths))
})

test_that("sort.Segment2D() decreasing = TRUE gives closest first", {
	p1 <- as_coord2d(x = c(0, 2, 1), y = c(0, 0, 2))
	p2 <- as_coord2d(x = c(2, 1, 0), y = c(0, 2, 0))
	s <- as_segment2d(p1, p2 = p2)
	s_asc <- sort(s, alpha = degrees(45))
	s_desc <- sort(s, decreasing = TRUE, alpha = degrees(45))
	expect_equal(
		painter_depth(s_asc, scale = 1, alpha = degrees(45)),
		rev(painter_depth(s_desc, scale = 1, alpha = degrees(45)))
	)
})
