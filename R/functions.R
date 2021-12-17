#' @title General functions
#' 
#' @name general_functions
#' 
#' @description 
#' These functions manipulate segmetric objects.
#' * `sm_area()` returns a vector of areas, one for each polygon.
#' * `sm_centroid()` returns the centroids of the given polygons.
#' * `sm_intersection()` returns the intersection of the given simple features.
#' * `sm_subset_union()` returns the union of the given simple features.
#' * `sm_rbind()` returns the merge of unique simple features.
#' 
#' @param s,s1,s2 Either a `ref_sf`, a `seg_sf`, or a `subset_sf` object 
#' (inherits from `sf`).
#' @param order   A `subset_sf`. This argument arranges the returned values 
#' according to the object passed here.
#' @param touches A logical. Is the border part of the intersection?
#' @param ...     Set of `sf` objects of type subset `sf`.
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

.norm_left <- function(x, y) {
    (x - y) / x
}

.norm_right <- function(x, y) {
    1 - x / y
}
