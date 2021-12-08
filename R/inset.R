
#' @rdname subset_handling_functions
#' @export
`%inset%` <- function(x, y) {
    
    UseMethod("%inset%", x)
}

#' @rdname subset_handling_functions
#' @export
`%inset%.ref_sf` <- function(x, y) {
    
    sm_id(x, inset = y)
}

#' @rdname subset_handling_functions
#' @export
`%inset%.seg_sf` <- function(x, y) {
    
    sm_id(x, inset = y)
}

#' @rdname subset_handling_functions
#' @export
`%inset%.subset_sf` <- function(x, y) {
    
    .subset_check(x)
    .subset_check(y, allowed_types = "subset_sf")
    
    which(sm_id(x) %in% sm_id(y))
}
