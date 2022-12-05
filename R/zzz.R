#' @importFrom magrittr `%>%`
#' @export
magrittr::`%>%`

#' @importFrom graphics legend
#' @importFrom grDevices hcl.colors
#' @importFrom stats quantile
NULL

# package environment
.db_env <- new.env()

# package on load
.onLoad <- function(libname, pkgname) {
    sm_options(digits = 10)
    .db_registry()
}
