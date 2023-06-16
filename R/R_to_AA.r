# Rotation matrix to Axis-angle representation
# https://en.wikipedia.org/wiki/Axis-angle_representation

#' Convert from 3D rotation matrix to axis-angle representation.
#'
#' `rotate3d_to_AA()` converts from (post-multiplied) rotation matrix
#' to an axis-angle representation of 3D rotations.
#'
#' @examples
#'  # axis-angle representation of 90 degree rotation about the x-axis
#'  rotate3d_to_AA(rotate3d("x-axis", 90, unit = "degrees"))
#'
#'  # find Axis-Angle representation of first rotating about x-axis 180 degrees
#'  # and then rotating about z-axis 45 degrees
#'  R <- rotate3d("x-axis", 180, unit = "degrees") %*%
#'         rotate3d("z-axis", 45, unit = "degrees")
#'  AA <- rotate3d_to_AA(R)
#'
#'  # Can use `rotate3d()` to convert back to rotation matrix representation
#'  all.equal(R, do.call(rotate3d, AA))
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Axis-angle_representation} for more details
#'   about the Axis-angle representation of 3D rotations.
#'   [rotate3d()] can be used to convert from an axis-angle representation to a rotation matrix.
#' @param mat 3D rotation matrix (post-multiplied).
#'            If you have a pre-multiplied rotation matrix
#'            simply transpose it with [t()] to get a post-multiplied rotation matrix.
#' @inheritParams angle
#' @export
rotate3d_to_AA <- function(mat = diag(4), unit = getOption("affiner_angular_unit", "degrees")) {
    unit <- standardize_angular_unit(unit)
    if (!all(dim(mat) == c(3, 3)) && !is_transform3d(mat))
        mat <- transform3d(mat)
    R <- mat[1:3, 1:3]

    theta <- arccosine(0.5 * (trace(R) - 1), unit = unit)
    if (all(is_congruent(R, diag(3)))) { # no rotation
        theta <- angle(0, unit)
        e <- c(0, 0, 1)
    } else if (is_congruent(theta, angle(180, "degrees"))) { # 180 degree rotation
        theta <- angle(from_piradians(1, unit), unit)
        B <- 0.5 * (R + diag(3))
        e <- sqrt(diag(B))
        sB <- sign(B)
        if (all(is_congruent(sB, ppn))) {
            e[3] <- -e[3]
            theta <- angle(from_piradians(-1, unit), unit)
        } else if (all(is_congruent(sB, pnp))) {
            e[2] <- -e[2]
        } else if (all(is_congruent(sB, npp))) {
            e[1] <- -e[1]
        }
    } else {
        e <- numeric(3)
        e[1] <- R[3,2] - R[2,3]
        e[2] <- R[1,3] - R[3,1]
        e[3] <- R[2,1] - R[1,2]
        e <- e / (2 * sin(-theta))
    }
    if (e[3] < 0) { # Force z-axis element positive
        e <- -e
        theta <- -theta
    }
    list(axis = as_coord3d(e[1], e[2], e[3]),
         theta = theta)
}

# Sign matrices for "B" matrix used to tell signs for axis unit vector (up to sign ambiguity) when angle = 180 degrees
# signs for ppp and nnn don't need to be changed
# ppn and npp
ppn <- matrix(c(1, 1, -1,
                1, 1, -1,
                -1, -1, 1), ncol = 3, byrow = TRUE)
# pnp and npn
pnp <- matrix(c(1, -1, 1,
                -1, 1, -1,
                1, -1, 1), ncol = 3, byrow = TRUE)
# pnn and npp
npp <- matrix(c(1, -1, -1,
                -1, 1, 1,
                -1, 1, 1), ncol = 3, byrow = TRUE)

# trace of a (square) matrix
trace <- function(m) sum(diag(m))
