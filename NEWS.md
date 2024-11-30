affiner 0.1.2 (development)
===========================

* The Details sections for `help("affineGrob")` and `help("isocubeGrob")` now document
  which graphics devices are known to support the affine transformation feature (#56).

affiner 0.1.1
=============

Initial features
----------------

* `affineGrob()` and `grid.affine()` provide wrappers around `grid::defineGrob()` and `grid::useGrob()`

  + `isocubeGrob()` and `grid.isocube()` provides a convenience wrapper for the isometric cube case.

* `affine_settings()` computes `grid` affine transformation feature viewports and transformation functions

  + Available as a "standalone" file that can be copied
    over into other R packages under the permissive [Unlicense](https://unlicense.org/).

* `angle()` creates angle vector S3 classes that allow users to use whichever angular unit is most convenient for them:

  + Supports "degrees", "radians", "half-turns" (aka "pi-radians"), (full) "turns", and "gradians" units.
  + `is_angle()` tests whether the input is an angle vector.
  + `as_angle()` casts objects to angle vectors.
  + `degrees()`, `gradians()`, `pi_radians()`, `radians()`, and `turns()` are convenience wrappers around
    `as_angle()` for those commonly used angular units.
  + `is_congruent()` is a generic S3 method which tests whether two R objects are "congruent".
    The `is_congruent()` method for angle vectors tests whether two angles are congruent.
  + `angular_unit()` can be used to get/set the angular unit of angle vectors.
  + The default angular unit can be adjusted locally/globally by setting the "affiner\_angular\_unit" option.
    e.g. `options(affiner_angular_unit = "turns")`.
  + `sine()`, `cosine()`, `tangent()`, `secant()`, `cosecant()`, `cotangent()`,
    `arcsine()`, `arccosine()`, `arctangent()`, `arcsecant()`, `arccosecant()`, and `arccotangent()`
    are angle vector aware trigonometric functions.
  + We implement methods for several base generics (plus as a numeric vector it inherits support for several more).
    Some notes:

    - If both objects in a mathematical operation (in particular `+` and `-`) or a `c()` combining operation
      are angle vectors then we coerce the second one to use the same angular unit as the first one.
    - `as.numeric()` takes a `unit` argument which can be used to convert angles into other angular units
      e.g. `angle(x, "degrees") |> as.numeric("radians")` to cast a numeric vector `x` from degrees to radians.
    - `abs()` will calculate the angle modulo full turns.

* `Coord1D`, `Coord2D`, and `Coord3D` are (Cartesian) coordinate R6 classes

  + `is_coord1d()`, `is_coord2d()`, and `is_coord3d()` test whether objects are `Coord1D`, `Coord2D`, or `Coord3D` R6 classes
  + `as_coord1d()`, `as_coord2d()`, and `as_coord3d()` cast objects to `Coord1D`, `Coord2D`, or `Coord3D` R6 classes
  + Several mathematical operations are supported for `Coord1D`, `Coord2D`, or `Coord3D` R6 classes

    + `*` either applies a "dot" product (if multiplying another `Coord1D`, `Coord2D`, or `Coord3D` object)
          or a "scaling" transformation (if multiplying a numeric value)
    + `/` applies a "scaling" transformation
    + Unary `-` applies a "scaling" transformation whereas 
      binary `-` and `+` apply a "translation" transformation

  + Additional S3 methods:

    - `abs()` computes Euclidean norm
    - `convex_hull2d()` computes convex hull (currently just for `Coord2D` vectors)
    - `cross_product3d()` computes a cross product between `Coord3D` vectors
    - `distance1d()`, `distance2d()`, and `distance3d()` computes Euclidean distances
    - `mean()` computes centroids of coordinates
    - `normal2d()` computes `Coord2D` normals
    - `normal3d()` computes `Coord3D` normals
    - `plot()` and `points()` plots `Coord1D` and `Coord2D` coordinates using base graphics.
      If the suggested `{ggplot2}` package is installed one may also use `autolayer()`
      to plot `Coord1D` and `Coord2D` points.
      If the suggested `{rgl}` package is installed one may also use `plot3d()` to
      plot `Coord3D` points (or straightforwardly use the primitive `points3d()`).
    - `range()` computes axis-aligned ranges

* `Point1D`, `Line2D`, and `Plane3D` R6 classes

  + `as_point1d()` casts objects to `Point1D` R6 classes
  + `as_line2d()` casts objects to `Line2D` R6 classes
  + `as_plane3d()` casts objects to `Plane3D` R6 classes
  + `is_point1d()` tests whether objects are `Point1D` R6 classes
  + `is_line2d()` tests whether objects are `Line2D` R6 classes
  + `is_plane3d()` tests whether objects are `Plane3D` R6 classes

* `transform1d()`, `transform2d()`, and `transform3d()` create 1D/2D/3D affine transformation matrix S3 classes

  + `is_transform1d()`,`is_transform2d()`, and `is_transform3d()` test if`transform1d()`, `transform2d()`, or `transform3d()` objects.
  + `as_transform1d()`, `as_transform2d()`, and `as_transform3d()` cast objects to `transform1d()`, `transform2d()`, or `transform3d()` objects.
  + `permute2d()` and `permute3d()` transformation matrices permutes coordinate axes.
  + `project1d()`, `project2d()`, and `project3d()` create projection matrices.
  + `reflect1d()`, `reflect2d()` and `reflect3d()` create reflection affine transformation matrices.
  + `rotate2d()` and `rotate3d()` create rotation affine transformation matrices.
    `rotate3d_to_AA()` converts from 3D rotation matrix to axis-angle representation.
  + `scale1d()`, `scale2d()`, and `scale3d()` create scaling affine transformation matrices.
  + `shear2d()` and `shear3d()` create shearing affine transformation matrices.
  + `translate1d()`, `translate2d()`, and `translate3d()` create translation affine transformation matrices.

* `{affiner}` supports the following options settable by `base::options()`:

  + `affiner_angular_unit`: The default for the `unit` argument used by `angle()` and `as_angle()`.
    The default for this option is "degrees".
  + `affiner_grid_unit`: The default for the `unit` argument used by `affine_settings()`.
    The default for this option is `inches`.
  + These options can be queried with the convenience function `affiner_options()`.
