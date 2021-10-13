
ref_id <- function(x) {
    check_ref_sf(x, allow_universe = TRUE)
    
    x$ref_id
}

seg_id <- function(x) {
    check_seg_sf(x, allow_universe = TRUE)
    
    x$seg_id
}

universe_id <- function(x) {
    check_universe_sf(x)
    
    paste0(x$ref_id, ";", x$seg_id)
}

id <- function(x) {
    
    UseMethod("id", x)
}

id.ref_sf <- function(x) {
    
    ref_id(x)
}

id.seg_sf <- function(x) {
    
    seg_id(x)
}

id.universe_sf <- function(x) {
    
    universe_id(x)
}

distinct <- function(x) {

    id <- id(x)
    
    suppressWarnings(x[match(unique(id), id),])
}
