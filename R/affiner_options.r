#' Get affiner options
#'
#' `affiner_options()` returns the `affiner` package's global options.
#'
#' @param ... `affiner` package options using `name = value`.  
#'            The return list will use any of these instead of the current/default values.
#' @param default If `TRUE` return the default values instead of current values.
#' @return A list of option values.
#'         Note this function **does not** set option values itself but
#'         this list can be passed to [options()], [withr::local_options()], or [withr::with_options()].
#' @examples
#'   affiner_options()
#'
#'   affiner_options(default = TRUE)
#'
#'   affiner_options(affiner_angular_unit = "pi-radians")
#' @seealso [affiner] for a high-level description of relevant global options.
#' @export
affiner_options <- function(..., default = FALSE) {
    afo <- list(affiner_angular_unit = "degrees",
                affiner_grid_unit = "inches")
    l <- list(...)
    stopifnot(all(names(l) %in% names(afo)))
    if (isFALSE(default)) {
       for (n in names(afo)) {
           afo[n] <- list(getOption(n, afo[[n]]))
       }
    }
    if (length(names(l))) {
        for (n in names(l)) {
            afo[n] <- l[n]
        }
    }
    afo
}
