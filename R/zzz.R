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
#' @param rhs right hand side parameter.
#' 
#' @export
NULL

#' @importFrom graphics legend
NULL

# package environment
.db_env <- new.env()

# package on load
.onLoad <- function(libname, pkgname) {
    .db_registry()
}
