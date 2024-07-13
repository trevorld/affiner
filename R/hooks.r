# nocov start
.onLoad <- function(libname, pkgname) {
    if (getRversion() >= "4.3.0") {
        s3_register("base::%*%", "at_matrix", `%*%.at_matrix`)
    }
    s3_register("ggplot2::autolayer", "Coord1D", autolayer.Coord1D)
    s3_register("ggplot2::autolayer", "Coord2D", autolayer.Coord2D)
    s3_register("ggplot2::autolayer", "Point1D", autolayer.Point1D)
    s3_register("ggplot2::autolayer", "Line2D", autolayer.Line2D)
    s3_register("rgl::plot3d", "Coord3D", plot3d.Coord3D)
}
# nocov end
