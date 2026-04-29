test_that("as_polygon2d()", {
	# from Coord2D
	v <- as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
	p <- as_polygon2d(v)
	expect_r6_class(p, "Polygon2D")
	expect_true(is_polygon2d(p))
	expect_true(is_coord2d(p)) # Polygon2D is a Coord2D subclass
	expect_false(is_polygon2d(v))
	expect_true(p$is_convex)
	expect_equal(length(p), 4L)

	# from numeric
	p2 <- as_polygon2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
	expect_r6_class(p2, "Polygon2D")

	# explicit convex override
	p3 <- as_polygon2d(v, convex = TRUE)
	expect_true(p3$is_convex)
	p4 <- as_polygon2d(v, convex = FALSE)
	expect_false(p4$is_convex)

	# print
	expect_snapshot(print(p))
})

test_that("as_polygon2d.Polygon2D()", {
	p <- as_polygon2d(as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)))
	expect_identical(as_polygon2d(p), p)
	expect_false(as_polygon2d(p, convex = FALSE)$is_convex)
})

test_that("Polygon2D normals", {
	# Unit square vertices CCW
	v <- as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
	p <- as_polygon2d(v)
	norms <- p$normals
	expect_r6_class(norms, "Coord2D")
	expect_false(is_polygon2d(norms)) # normals are plain Coord2D, not Polygon2D
	expect_equal(length(norms), 4L)
	expect_equal(abs(norms), rep(1, 4L))

	# Cached: second access returns same object
	expect_identical(p$normals, norms)

	# Cache invalidated after transform
	p$rotate(degrees(45))
	expect_false(identical(p$normals, norms))
})

test_that("Polygon2D$convex_hull", {
	# Convex polygon: convex_hull returns itself
	v <- as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
	p <- as_polygon2d(v, convex = TRUE)
	expect_identical(p$convex_hull, p)

	# Concave polygon (star-like): hull is a different polygon
	# Simple concave: a "C" shape or indented square
	vv <- as_coord2d(
		x = c(0, 0.5, 0.5, 1, 1, 0.5, 0.5, 0),
		y = c(0, 0, 0.4, 0.4, 0.6, 0.6, 1, 1)
	)
	pc <- as_polygon2d(vv, convex = FALSE)
	hull <- pc$convex_hull
	expect_r6_class(hull, "Polygon2D")
	expect_true(hull$is_convex)
	expect_true(length(hull) <= length(pc))

	# Cached: second call returns same object
	expect_identical(pc$convex_hull, hull)

	# Cache invalidated after transform
	pc$rotate(degrees(45))
	expect_false(identical(pc$convex_hull, hull))
})

test_that("convex_hull2d() returns Polygon2D", {
	# From Coord2D
	pts <- as_coord2d(x = rnorm(20), y = rnorm(20))
	hull <- convex_hull2d(pts)
	expect_r6_class(hull, "Polygon2D")
	expect_true(hull$is_convex)
	expect_true(is_coord2d(hull)) # backwards compatible

	# From convex Polygon2D: returns itself
	v <- as_coord2d(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
	p <- as_polygon2d(v, convex = TRUE)
	expect_identical(convex_hull2d(p), p)
})

test_that("Polygon2D convex auto-detection", {
	# Convex (CCW square)
	expect_true(
		as_polygon2d(as_coord2d(
			x = c(0, 1, 1, 0),
			y = c(0, 0, 1, 1)
		))$is_convex
	)

	# Convex (CW square)
	expect_true(
		as_polygon2d(as_coord2d(
			x = c(0, 0, 1, 1),
			y = c(0, 1, 1, 0)
		))$is_convex
	)

	# Degenerate: fewer than 3 vertices → FALSE
	expect_false(as_polygon2d(as_coord2d(x = 1:2, y = 1:2))$is_convex)

	# Concave C-shape
	expect_false(
		as_polygon2d(as_coord2d(
			x = c(0, 0.5, 0.5, 1, 1, 0.5, 0.5, 0),
			y = c(0, 0, 0.4, 0.4, 0.6, 0.6, 1, 1)
		))$is_convex
	)
})

test_that("has_overlap2d() polygon-polygon", {
	sq1 <- as_polygon2d(as_coord2d(
		x = c(0, 1, 1, 0),
		y = c(0, 0, 1, 1)
	))
	sq2 <- as_polygon2d(as_coord2d(
		x = c(0.5, 1.5, 1.5, 0.5),
		y = c(0.5, 0.5, 1.5, 1.5)
	))
	sq3 <- as_polygon2d(as_coord2d(
		x = c(2, 3, 3, 2),
		y = c(2, 2, 3, 3)
	))

	expect_true(has_overlap2d(sq1, sq2))
	expect_false(has_overlap2d(sq1, sq3))

	# Touching corners: strictly no non-zero-area overlap
	sq4 <- as_polygon2d(as_coord2d(
		x = c(1, 2, 2, 1),
		y = c(1, 1, 2, 2)
	))
	expect_false(has_overlap2d(sq1, sq4))
})

test_that("has_overlap2d() polygon-circle", {
	sq <- as_polygon2d(as_coord2d(
		x = c(0, 1, 1, 0),
		y = c(0, 0, 1, 1)
	))
	circ_in <- as_ellipse2d(as_coord2d(0.5, 0.5), r = 0.3)
	circ_out <- as_ellipse2d(as_coord2d(3, 3), r = 0.3)
	circ_corner <- as_ellipse2d(as_coord2d(1.5, 1.5), r = 0.3)

	expect_true(has_overlap2d(sq, circ_in))
	expect_false(has_overlap2d(sq, circ_out))
	expect_false(has_overlap2d(sq, circ_corner))

	# Dispatching from Circle2D side
	expect_true(has_overlap2d(circ_in, sq))
	expect_false(has_overlap2d(circ_out, sq))

	# Vectorized circles
	circs <- as_ellipse2d(
		x = c(0.5, 3),
		y = c(0.5, 3),
		r = c(0.3, 0.3)
	)
	expect_equal(has_overlap2d(sq, circs), c(TRUE, FALSE))
})

test_that("has_overlap2d() concave polygon returns NA with warning", {
	# Concave polygon (C-shape)
	vc <- as_coord2d(
		x = c(0, 0.5, 0.5, 1, 1, 0.5, 0.5, 0),
		y = c(0, 0, 0.4, 0.4, 0.6, 0.6, 1, 1)
	)
	pc <- as_polygon2d(vc, convex = FALSE)
	sq <- as_polygon2d(as_coord2d(
		x = c(0.3, 0.7, 0.7, 0.3),
		y = c(0.3, 0.3, 0.7, 0.7)
	))

	# Overlapping bounding boxes + convex hulls → NA with warning
	expect_warning(res <- has_overlap2d(pc, sq), "concave")
	expect_true(is.na(res))

	# Clearly disjoint → FALSE without warning
	sq_far <- as_polygon2d(as_coord2d(
		x = c(5, 6, 6, 5),
		y = c(5, 5, 6, 6)
	))
	expect_false(has_overlap2d(pc, sq_far))
})

test_that("has_overlap2d() concave polygon and circle (bbox_overlap_pc)", {
	pc <- as_polygon2d(
		as_coord2d(
			x = c(0, 0.5, 0.5, 1, 1, 0.5, 0.5, 0),
			y = c(0, 0, 0.4, 0.4, 0.6, 0.6, 1, 1)
		),
		convex = FALSE
	)
	# Circle outside bbox → FALSE via bbox_overlap_pc
	expect_false(has_overlap2d(pc, as_ellipse2d(as_coord2d(5, 5), r = 0.3)))
	# Circle inside bbox, overlapping convex hull → NA with warning
	expect_warning(
		res <- has_overlap2d(pc, as_ellipse2d(as_coord2d(0.5, 0.5), r = 0.3)),
		"concave"
	)
	expect_true(is.na(res))
})
