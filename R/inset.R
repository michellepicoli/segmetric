
`%inset%` <- function(x, y) {
    
    UseMethod("%inset%", x)
}

`%inset%.ref_sf` <- function(x, y) {
    
    which(ref_id(x) %in% ref_id(y))
}

`%inset%.seg_sf` <- function(x, y) {
    
    which(seg_id(x) %in% seg_id(y))
}

`%inset%.universe_sf` <- function(x, y) {
    
    which(universe_id(x) %in% universe_id(y))
}
