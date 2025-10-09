# {roxygen2} doesn't allow R6 class documentation to
# inherit from S3 method param documentation

r2i_transform1d_mat <- paste(
	"A 2x2 matrix representing a post-multiplied affine transformation matrix.",
	"The last **column** must be equal to `c(0, 1)`.",
	"If the last **row** is `c(0, 1)` you may need to transpose it",
	"to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.",
	"If a 1x1 matrix we'll quietly add a final column/row equal to `c(0, 1)`.",
	sep = "\n"
)

r2i_transform2d_mat <- paste(
	"A 3x3 matrix representing a post-multiplied affine transformation matrix.",
	"The last **column** must be equal to `c(0, 0, 1)`.",
	"If the last **row** is `c(0, 0, 1)` you may need to transpose it",
	"to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.",
	"If a 2x2 matrix (such as a 2x2 post-multiplied 2D rotation matrix)",
	"we'll quietly add a final column/row equal to `c(0, 0, 1)`.",
	sep = "\n"
)

r2i_transform3d_mat <- paste(
	"A 4x4 matrix representing a post-multiplied affine transformation matrix.",
	"The last **column** must be equal to `c(0, 0, 0, 1)`.",
	"If the last **row** is `c(0, 0, 0, 1)` you may need to transpose it",
	"to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.",
	"If a 3x3 matrix (such as a 3x3 post-multiplied 3D rotation matrix)",
	"we'll quietly add a final column/row equal to `c(0, 0, 0, 1)`.",
	sep = "\n"
)

r2i_transform2d_permutation <- 'Either "xy" (no permutation) or "yx" (permute x and y axes)'

r2i_transform3d_permutation <- paste(
	'Either "xyz" (no permutation), "xzy" (permute y and z axes),',
	'"yxz" (permute x and y axes), "yzx" (x becomes z, y becomes x, z becomes y),',
	'"zxy" (x becomes y, y becomes z, z becomes x), "zyx" (permute x and z axes)',
	sep = "\n"
)

r2i_transform3d_axis <- paste(
	"A [Coord3D] class object or one that can coerced to one by `as_coord3d(axis, ...)`.",
	"The `axis` represents the axis to be rotated around.",
	sep = "\n"
)

r2i_transform_theta <- "An [angle()] object of length one or an object coercible to one by `as_angle(theta, ...)`."

r2i_transform2d_scale <- paste(
	"Oblique projection scale factor.",
	"A degenerate `0` value indicates an orthogonal projection.",
	sep = "\n"
)

r2i_transform3d_scale <- paste(
	"Oblique projection foreshortening scale factor.",
	"A (degenerate) `0` value indicates an orthographic projection.",
	"A value of `0.5` is used by a \\dQuote{cabinet projection}",
	"while a value of `1.0` is used by a \\dQuote{cavalier projection}.",
	sep = "\n"
)

r2i_transform3d_alpha <- paste(
	"Oblique projection angle (the angle the third axis is projected going off at).",
	"An [angle()] object or one coercible to one with `as_angle(alpha, ...)`.",
	"Popular angles are 45 degrees, 60 degrees, and `arctangent(2)` degrees.",
	sep = "\n"
)

r2i_transform_x_scale <- "Scaling factor to apply to x coordinates"
r2i_transform_y_scale <- "Scaling factor to apply to y coordinates"
r2i_transform_z_scale <- "Scaling factor to apply to z coordinates"

r2i_transform2d_xy_shear <- "Horizontal shear factor: `x = x + xy_shear * y`"
r2i_transform2d_yx_shear <- "Vertical shear factor: `y = yx_shear * x + y`"

r2i_transform3d_xy_shear <- "Shear factor: `x = x + xy_shear * y + xz_shear * z`"
r2i_transform3d_xz_shear <- "Shear factor: `x = x + xy_shear * y + xz_shear * z`"
r2i_transform3d_yx_shear <- "Shear factor: `y = yx_shear * x + y + yz_shear * z`"
r2i_transform3d_yz_shear <- "Shear factor: `y = yx_shear * x + y + yz_shear * z`"
r2i_transform3d_zx_shear <- "Shear factor: `z = zx_shear * x + zy_shear * y + z`"
r2i_transform3d_zy_shear <- "Shear factor: `z = zx_shear * x + zy_shear * y + z`"

r2i_transform1d_point <- paste(
	"A [Point1D] object of length one representing the point",
	"you with to reflect across or project to or an object coercible to one by `as_point1d(point, ...)`",
	' such as "origin".',
	sep = "\n"
)
r2i_transform2d_line <- paste(
	"A [Line2D] object of length one representing the line",
	"you with to reflect across or project to or an object coercible to one by `as_line2d(line, ...)`",
	' such as "x-axis" or "y-axis".',
	sep = "\n"
)
r2i_transform3d_plane <- paste(
	'A [Plane3D] object of length one representing the plane',
	'you wish to reflect across or project to or an object coercible to one using `as_plane3d(plane, ...)`',
	'such as "xy-plane", "xz-plane", or "yz-plane".',
	sep = "\n"
)

r2i_transform1d_x <- "A [Coord1D] object of length one or an object coercible to one by `as_coord1d(x, ...)`."
r2i_transform2d_x <- "A [Coord2D] object of length one or an object coercible to one by `as_coord2d(x, ...)`."
r2i_transform3d_x <- "A [Coord3D] object of length one or an object coercible to one by `as_coord3d(x, ...)`."

affine_transformation_support <- paste(
	"Not all graphics devices provided by `grDevices` or other R packages support the [affine transformation feature introduced in R 4.2](https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html).",
	"If `isTRUE(getRversion() >= '4.2.0')` then the active graphics device should support this feature if `isTRUE(grDevices::dev.capabilities()$transformations)`.",
	"In particular the following graphics devices should support the affine transformation feature:",
	"",
	"* R's [grDevices::pdf()] device",
	"* R's 'cairo' devices e.g. [grDevices::cairo_pdf()], `grDevices::png(type = 'cairo')`, [grDevices::svg()], `grDevices::x11(type = 'cairo')`, etc. If `isTRUE(capabilities('cairo'))` then R was compiled with support for the 'cairo' devices .",
	"* R's 'quartz' devices (since R 4.3.0) e.g. [grDevices::quartz()], `grDevices::png(type = 'quartz')`, etc. If `isTRUE(capabilities('aqua'))` then R was compiled with support for the 'quartz' devices (generally only `TRUE` on macOS systems).",
	"* `ragg`'s devices (since v1.3.0) e.g. [ragg::agg_png()], [ragg::agg_capture()], etc.",
	sep = "\n"
)
