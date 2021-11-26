
sm_area <- function(s, order = NULL) {
    # s checked
    
    .subset_check(s)
    if (!is.null(order))
        .subset_check(order, allowed_types = "subset_sf")
    
    res <- suppressWarnings(suppressMessages(
        sf::st_area(s)
    ))
    
    if (inherits(res, "units"))
        res <- units::drop_units(res)
    
    if (!is.null(order))
        return(res[sm_id(s, inset = order)])
    
    return(res)
}

sm_filter_intersects <- function(s1, s2, order = NULL) {
    
}

sm_centroid <- function(s) {
    # s checked
    
    .subset_check(s, allowed_types = c("ref_sf", "seg_sf"))
    
    res <- suppressWarnings(suppressMessages(
        sf::st_centroid(s, of_largest_polygon = FALSE)
    ))
    
    class(res) <- class(s)
    
    return(res)
}

sm_intersections <- function(s1, s2, touches = TRUE) {
    # s checked
    
    .subset_check(s1, allowed_types = c("ref_sf", "seg_sf"))
    .subset_check(s2, allowed_types = c("ref_sf", "seg_sf"))
    
    res <- suppressWarnings(suppressMessages({
        sf::st_intersection(x = s1, y = s2)
    }))
    
    # filter only polygons
    if (!touches) {
        area <- sm_area(res)
        
        res <- suppressWarnings(suppressMessages(
            res[area > 0,]
        ))
    }
    
    class(res) <- c("subset_sf", class(res))
    
    return(res)
}

sm_union <- function(s1, s2, order) {
    # s checked

    .subset_check(s1)
    .subset_check(s2)
    .subset_check(order)

    dplyr::bind_rows(lapply(seq_len(nrow(order)), function(i) {
        suppressWarnings(suppressMessages({
            sf::st_union(x = x[ref_id(x[i,]),],
                         y = y[seg_id(x[i,]),])
        }))
    }))
}

bind_all <- function(...) {
    
    dots <- list(...)
    stopifnot(all(vapply(dots, inherits, logical(1), "universe_sf")))

    res <- suppressWarnings(
        do.call(rbind, args = dots)
    )
    
    id <- paste0(res[["ref_id"]], ";", res[["seg_id"]])
    
    suppressWarnings(res[match(unique(id), id),])
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
