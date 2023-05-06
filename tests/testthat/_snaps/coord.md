# print.coord2d()

    Code
      print(p1, usage = TRUE)
    Output
      <coord2d[3]>
        Public Methods:
          clone()
          print(n = NULL, usage = getOption("affiner_print_usage", FALSE))
          project(theta = angle(0), ..., scale = 0)
          reflect(theta = angle(0), ...)
          rotate(theta = angle(0), ...)
          scale(x_scale = 1, y_scale = x_scale)
          shear(xy_shear = 0, yx_shear = 0)
          transform(mat = transform2d())
          translate(vec = coord2d(0, 0), ...)
        Active Bindings:
          x
          y
          xyw
        Homogeneous Cartesian Coordinates:
           x y w
      [1,] 2 3 1
      [2,] 5 4 1
      [3,] 7 6 1

---

    Code
      print(p1, usage = TRUE, n = 0)
    Output
      <coord2d[3]>
        Public Methods:
          clone()
          print(n = NULL, usage = getOption("affiner_print_usage", FALSE))
          project(theta = angle(0), ..., scale = 0)
          reflect(theta = angle(0), ...)
          rotate(theta = angle(0), ...)
          scale(x_scale = 1, y_scale = x_scale)
          shear(xy_shear = 0, yx_shear = 0)
          transform(mat = transform2d())
          translate(vec = coord2d(0, 0), ...)
        Active Bindings:
          x
          y
          xyw

---

    Code
      print(p1, usage = FALSE)
    Output
      <coord2d[3]>
           x y w
      [1,] 2 3 1
      [2,] 5 4 1
      [3,] 7 6 1

---

    Code
      print(coord2d(), usage = FALSE)
    Output
      <coord2d[0]>

# print.coord3d()

    Code
      print(p1, usage = TRUE)
    Output
      <coord3d[3]>
        Public Methods:
          clone()
          permute(permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy"))
          print(n = NULL, usage = getOption("affiner_print_usage", FALSE))
          project(normal = as_coord3d("xy-plane"), ...,  scale = 0, alpha = angle(45, "degrees"))
          reflect(normal = as_coord3d("xy-plane"), ...)
          rotate(axis = as_coord3d("z-axis"), theta = angle(0), ...)
          scale(x_scale = 1, y_scale = x_scale, z_scale = x_scale)
          shear(xy_shear = 0, yx_shear = 0, yx_shear = 0, yz_shear = 0,
                zx_shear = 0, zy_shear = 0)
          transform(mat = transform3d())
          translate(vec = coord3d(0, 0, 0), ...)
        Active Bindings:
          x
          y
          z
          xyzw
        Homogeneous Cartesian Coordinates:
           x y z w
      [1,] 2 3 1 1
      [2,] 5 4 1 1
      [3,] 7 6 1 1

---

    Code
      print(p1, usage = TRUE, n = 0)
    Output
      <coord3d[3]>
        Public Methods:
          clone()
          permute(permutation = c("xyz", "xzy", "yxz", "yzx", "zyx", "zxy"))
          print(n = NULL, usage = getOption("affiner_print_usage", FALSE))
          project(normal = as_coord3d("xy-plane"), ...,  scale = 0, alpha = angle(45, "degrees"))
          reflect(normal = as_coord3d("xy-plane"), ...)
          rotate(axis = as_coord3d("z-axis"), theta = angle(0), ...)
          scale(x_scale = 1, y_scale = x_scale, z_scale = x_scale)
          shear(xy_shear = 0, yx_shear = 0, yx_shear = 0, yz_shear = 0,
                zx_shear = 0, zy_shear = 0)
          transform(mat = transform3d())
          translate(vec = coord3d(0, 0, 0), ...)
        Active Bindings:
          x
          y
          z
          xyzw

---

    Code
      print(p1, usage = FALSE)
    Output
      <coord3d[3]>
           x y z w
      [1,] 2 3 1 1
      [2,] 5 4 1 1
      [3,] 7 6 1 1

---

    Code
      print(coord3d(), usage = FALSE)
    Output
      <coord3d[0]>

