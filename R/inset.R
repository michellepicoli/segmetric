
#' @export
`%inset%` <- function(x, y) {
    
    UseMethod("%inset%", x)
}

#' @export
`%inset%.ref_sf` <- function(x, y) {
    
    sm_id(x, inset = y)
}

#' @export
`%inset%.seg_sf` <- function(x, y) {
    
    sm_id(x, inset = y)
}

#' @export
`%inset%.subset_sf` <- function(x, y) {
    
    .subset_check(x)
    .subset_check(y, allowed_types = "subset_sf")
    
    which(sm_id(x) %in% sm_id(y))
}
