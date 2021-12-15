#' @title General functions
#' 
#' @name general_functions
#' 
#' @description 
#' These functions manipulate segmetric objects.
#' * `sm_area()` ...
#' * `sm_centroid()` ...
#' * `sm_intersection()` ...
#' * `sm_subset_union()` ...
#' * `sm_rbind()` ...
#' * `norm_left()` ...
#' * `norm_frac()` ...
#' * `norm_diff()` ...
#' @param s       A `sf` object. Either a reference, a segmentation, or a subset.
#' @param s1      A `sf` object. Either a reference, a segmentation, or a subset.
#' @param s2      A `sf` object. Either a reference, a segmentation, or a subset.
#' @param order   A `sf` object of type subset`sf`.
#' @param touches A logical. Is the border part of the intersection?
#' @param x       A numerator of a fraction.
#' @param y       A denominator of a fraction.
#' @param ...     Set of `sf` objects of type subset`sf`.
#' 
NULL

#' @rdname general_functions
#' @export
sm_area <- function(s, order = NULL) {
    # s checked
    
    .subset_check(s)
    if (!is.null(order))
        .subset_check(order, allowed_types = "subset_sf")
    
    area <- suppressWarnings(suppressMessages(
        sf::st_area(s)
    ))
    
    if (inherits(area, "units"))
        area <- units::drop_units(area)
    
    if (!is.null(order))
        return(area[sm_inset(s, order, return_index = TRUE)])
    
    area
}

#' @rdname general_functions
#' @export
sm_centroid <- function(s) {
    # s checked
    
    .subset_check(s, allowed_types = c("ref_sf", "seg_sf"))
    
    res <- suppressWarnings(suppressMessages(
        sf::st_centroid(s, of_largest_polygon = FALSE)
    ))
    
    class(res) <- class(s)
    
    res
}

#' @rdname general_functions
#' @export
sm_intersection <- function(s1, s2, touches = TRUE) {
    # s checked
    
    .subset_check(s1, allowed_types = c("ref_sf", "seg_sf"))
    .subset_check(s2, allowed_types = c("ref_sf", "seg_sf"))
    
    subset_sf <- suppressWarnings(suppressMessages({
        sf::st_intersection(x = s1, y = s2)
    }))
    
    class(subset_sf) <- c("subset_sf", class(subset_sf))
    
    # filter only polygons
    if (!touches) {
        area <- sm_area(subset_sf)
        
        subset_sf <- suppressWarnings(suppressMessages(
            subset_sf[area > 0,]
        ))
    }
    
    subset_sf
}

#' @rdname general_functions
#' @export
sm_subset_union <- function(s) {
    # s checked
    m <- sm_segmetric(s)
    sm_subset(
        m,
        subset_id = paste(sm_indirect(s), "union", sep = "_"),
        expr = {
            if (nrow(s) == 0)
                s
            else
                do.call(rbind, args = lapply(seq_len(nrow(s)), function(i) {
                    # TODO: optimize can be done by vectorizing union operation
                    # link to GEOS library CPP function GEOSUnion_r 
                    suppressWarnings(suppressMessages({
                        sf::st_union(x = sm_inset(sm_ref(m), s[i,]),
                                     y = sm_inset(sm_seg(m), s[i,]))
                    }))
                }))
        })
}

#' @rdname general_functions
#' @export
sm_rbind <- function(...) {
    
    subsets <- list(...)
    for (s in subsets) .subset_check(s, allowed_types = "subset_sf")
    
    result <- suppressWarnings(
        do.call(rbind, args = subsets)
    )
    
    class(result) <- unique(c("subset_sf", class(result)))
    
    id <- paste(result[["ref_id"]], result[["seg_id"]], sep = ",")
    result <- suppressWarnings(result[match(unique(id), id),])
    
    result
}

#' @rdname general_functions
#' @export
sm_norm_left <- function(x, y) {
    (x - y) / x
}

#' @rdname general_functions
#' @export
sm_norm_frac <- function(x, y) {
    1 - x / y
}
