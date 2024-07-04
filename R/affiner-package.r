#' @section Package options:
#' The following affiner function arguments may be set globally via [base::options()]:
#'  \describe{
#'    \item{affiner_angular_unit}{The default for the `unit` argument used by [angle()] and [as_angle()].
#'                                The default for this option is "degrees".}
#'    \item{affiner_grid_unit}{The default for the `unit` argument used by [affine_settings()].
#'                                The default for this option is "inches".}
#'  }
#'  The following `cli` options may also be of interest:
#'   \describe{
#'     \item{cli.unicode}{Whether UTF-8 character support should be assumed.
#'                        Along with [l10n_info()] used to determine the default of the
#'                        `use_unicode` argument of [format.angle()] and [print.angle()].}
#'   }
"_PACKAGE"
