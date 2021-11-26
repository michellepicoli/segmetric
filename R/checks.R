check_ref_sf <- function(x, allow_universe = TRUE) {
    classes <- "ref_sf"
    if (allow_universe)
        classes <- c("ref_sf", "universe_sf")
    stopifnot(inherits(x, classes))
    stopifnot(inherits(x, "sf"))
    stopifnot(c("ref_id") %in% names(x))
}

check_seg_sf <- function(x, allow_universe = TRUE) {
    classes <- "seg_sf"
    if (allow_universe)
        classes <- c("seg_sf", "universe_sf")
    stopifnot(inherits(x, classes))
    stopifnot(inherits(x, "sf"))
    stopifnot(c("seg_id") %in% names(x))
}

