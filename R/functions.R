area <- function(x, order = NULL) {
    
    stopifnot(inherits(x, "sf"))
    
    res <- suppressWarnings(suppressMessages(
        sf::st_area(x)
    ))
    
    if (inherits(res, "units"))
        res <- units::drop_units(res)
    
    if (!is.null(order))
        return(res[order])
    return(res)
}

centroid <- function(x) {
    
    stopifnot(inherits(x, "sf"))
    
    suppressWarnings(suppressMessages(
        sf::st_centroid(x, of_largest_polygon = FALSE)
    ))
}

intersection <- function(x, y, touches = TRUE) {
    
    stopifnot(inherits(x, "sf"))
    stopifnot(inherits(y, "sf"))
    
    res <- suppressWarnings(suppressMessages({
        sf::st_intersection(x = x, y = y)
    }))
    
    # filter only polygons
    if (!touches)
        res <- res[area(res) > 0,]
    
    class(res) <- c("universe_sf", class(res))
    
    res
}

union <- function(x, ref_sf, seg_sf) {
    
    stopifnot(inherits(x, "sf"))
    stopifnot(c("ref_id", "seg_id") %in% names(x))
    
    dplyr::bind_rows(lapply(seq_len(nrow(x)), function(i) {
        suppressWarnings(suppressMessages({
            sf::st_union(x = seg_sf[ref_id(x[i,]),], 
                         y = ref_sf[seg_id(x[i,]),])
        }))
    }))
}

bind_all <- function(...) {
    
    dots <- list(...)
    ref_class <- all(vapply(dots, inherits, logical(1), "ref_sf"))
    seg_class <- all(vapply(dots, inherits, logical(1), "seg_sf"))
    universe_class <- all(vapply(dots, inherits, logical(1), "universe_sf"))
    stopifnot(any(c(ref_class, seg_class, universe_class)))

    res <- suppressWarnings(
        do.call(rbind, args = dots)
    )
    
    distinct(res)
}

norm_left <- function(x, y) {
    (x - y) / x
}

norm_frac <- function(x, y) {
    1 - x / y
}

norm_diff <- function(x, y) {
    (x - y) / (x + y)
}
