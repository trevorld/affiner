#' @importFrom R6 R6Class
#' @importFrom utils hasName
NULL

# variant of `cli::is_utf8_output()` without the `knitr::is_latex_output()` check
is_utf8_output <- function() {
    opt <- getOption("cli.unicode", NULL)
    if (!is.null(opt)) {
        isTRUE(opt)
    } else {
        l10n_info()[["UTF-8"]]
    }
}
