affiner 0.1.0 (development)
===========================

Initial features
----------------

* `affineGrob()` and `grid.affine()` provide wrappers around `grid::defineGrob()` and `grid::useGrob()`
* `affine_settings()` computes `grid` affine transformation feature viewports and transformation functions

  + Available as a "standalone" file that can be copied
    over into other R packages under the permissive [Unlicense](https://unlicense.org/).

* `angle()` creates angle vector S3 classes that allow users to use whichever angular unit is most convenient for them:

  + Supports "degrees", "radians", "half-turns" (aka "pi-radians"), (full) "turns", and "gradians".
  + `is_angle()` tests whether the input is an angle vector.
  + `as_angle()` casts objects to angle vectors.
  + `degrees()`, `pi_radians()`, and `radians()` are convenience wrappers for those
    commonly used angular units.
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

* `Coord2D` and `Coord3D` are (Cartesian) coordinate R6 classes

  + `is_coord2d()` and `is_coord3d()` test whether objects are `Coord2D` or `Coord3D` R6 classes
  + `as_coord2d()` and `as_coord3d()` cast objects to `Coord2D` or `Coord3D` R6 classes
  + Several mathematical operations are supported for `Coord2D` or `Coord3D` R6 classes

    + `*` either applies a "dot" product (if multiplying another `Coord2D` or `Coord3D` object)
          or a "scaling" transformation (if multiplying a numeric value)
    + `/` applies a "scaling" transformation
    + Unary `-` applies a "scaling" transformation whereas 
      binary `-` and `+` apply a "translation" transformation

  + Additional S3 methods:

    - `abs()` computes Euclidean norm
    - `convex_hull()` computes convex hull (currently just for `coord2d()` objects)
    - `cross_product()` computes a cross product between `Coord3D` vectors
    - `mean()` computes centroids of coordinates
    - `normal2d()` computes `Coord2D` normals
    - `normal3d()` computes `Coord3D` normals

* `transform2d()` and `transform3d()` create 2D/3D affine transformation matrix S3 classes

  + `is_transform2d()` and `is_transform3d()` test if `transform2d()` or `transform3d()` objects.
  + `as_transform2d()` and `as_transform3d()` cast objects to `transform2d()` or `transform3d()` objects.
  + `permute2d()` and `permute3d()` transformation matrices permutes coordinate axes.
  + `project2d()` and `project3d()` create projection matrices.
  + `reflect2d()` and `reflect3d()` create reflection affine transformation matrices.
  + `rotate2d()` and `rotate3d()` create rotation affine transformation matrices.
    `rotate3d_to_AA()` converts from 3D rotation matrix to axis-angle representation.
  + `scale2d()` and `scale3d()` create scaling affine transformation matrices.
  + `shear2d()` and `shear3d()` create shearing affine transformation matrices.
  + `translate2d()` and `translate3d()` create translation affine transformation matrices.

* `{affiner}` supports the following options settable by `base::options()`:

  + `affiner_angular_unit`: The default for the `unit` argument used by `angle()` and `as_angle()`.
    The default for this option is "degrees".
  + `affiner_print_usage`: The default for the `usage` argument used by `coord2d()` and `coord3d()` objects' print method.
    The default for this option is `TRUE`.
