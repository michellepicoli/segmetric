
`%inset%` <- function(x, y) {
    
    UseMethod("%inset%", x)
}

`%inset%.ref_sf` <- function(x, y) {
    
    sm_id(x, inset = y)
}

`%inset%.seg_sf` <- function(x, y) {
    
    sm_id(x, inset = y)
}

`%inset%.subset_sf` <- function(x, y) {
    
    .subset_check(x)
    .subset_check(y, allowed_types = "subset_sf")
    
    id <- sm_id(x)
    
    inset_id <- sm_id(y)
        
    which(inset_id %in% id)
}
