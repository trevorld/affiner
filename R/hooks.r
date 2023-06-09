# nocov start
.onLoad <- function(libname, pkgname) {
    if (getRversion() >= "4.3.0") {
        s3_register("base::%*%", "at_matrix", `%*%.at_matrix`)
    }
}
# nocov end
