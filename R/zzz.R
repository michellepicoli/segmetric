#' @title Pipe
#'
#' @name %>%
#' 
#' @importFrom magrittr `%>%`
#' 
#' @description
#' Imported function from `magrittr`.
#'
#' @param lhs left hand side parameter.
#' @param rhs right hand side parameter, usually a function.
#' 
#' @return Return any object evaluated by function call `rhs(lhs, ...)`.
#' 
#' @export
NULL

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
