#' @title General functions
#' 
#' @name general_functions
#' 
#' @description 
#' These functions manipulate segmetric objects.
#' * `sm_area()` Return a vector of areas, one for each polygon.
#' * `sm_centroid()` Return the centroids of the given polygons.
#' * `sm_intersections()` Return the intersection of the given simple features.
#' * `sm_union()` Return the unio of the given simple features.
#' * `rbind_distinct()` Return the merge of unique simple features.
#' * `sm_norm_left()` Return the normalized value by x.
#' * `sm_norm_frac()` Return the normalized value by y.
#' @param s       A `sf` object. Either a reference, a segmentation, or a subset.
#' @param s1      A `sf` object. Either a reference, a segmentation, or a subset.
#' @param s2      A `sf` object. Either a reference, a segmentation, or a subset.
#' @param order   A `sf` object of type subset`sf`. This argument is equivalent 
#' to left join. The return of the function are ordered according to the object 
#' passed to this parameter.
#' 
#' @param touches A logical. Is the border part of the intersection?
#' @param x       A numerator of a fraction.
#' @param y       A denominator of a fraction.
#' @param ...     Set of `sf` objects of type subset`sf`.
#' 
#' 
NULL

#' @rdname general_functions
#' @export
sm_area <- function(s, order = NULL) {
    # s checked
    
    #.subset_check(s)
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

#' @rdname general_functions
#' @export
sm_centroid <- function(s) {
    # s checked
    
    .subset_check(s, allowed_types = c("ref_sf", "seg_sf"))
    
    res <- suppressWarnings(suppressMessages(
        sf::st_centroid(s, of_largest_polygon = FALSE)
    ))
    
    class(res) <- class(s)
    
    return(res)
}

#' @rdname general_functions
#' @export
sm_intersections <- function(s1, s2, touches = TRUE) {
    # s checked
    
    .subset_check(s1, allowed_types = c("ref_sf", "seg_sf"))
    .subset_check(s2, allowed_types = c("ref_sf", "seg_sf"))
    
    res <- suppressWarnings(suppressMessages({
        sf::st_intersection(x = s1, y = s2)
    }))
    
    class(res) <- c("subset_sf", class(res))
    
    # filter only polygons
    if (!touches) {
        area <- sm_area(res)
        
        res <- suppressWarnings(suppressMessages(
            res[area > 0,]
        ))
    }
    
    return(res)
}

#' @rdname general_functions
#' @export
sm_union <- function(s1, s2, order) {
    # s checked

    .subset_check(s1, allowed_types = c("ref_sf", "seg_sf"))
    .subset_check(s2, allowed_types = c("ref_sf", "seg_sf"))
    .subset_check(order, allowed_types = "subset_sf")

    if (nrow(order) == 0)
        return(order)
    
    do.call(rbind, args = lapply(seq_len(nrow(order)), function(i) {
        suppressWarnings(suppressMessages({
            sf::st_union(x = s1[sm_id(s1, inset = order[i,]),],
                         y = s2[sm_id(s2, inset = order[i,]),])
        }))
    }))
}

#' @rdname general_functions
#' @export
rbind_distinct <- function(...) {
    
    subsets <- list(...)
    for (s in subsets) .subset_check(s, allowed_types = "subset_sf")
    
    res <- suppressWarnings(
        do.call(rbind, args = subsets)
    )
    
    class(res) <- unique(c("subset_sf", class(res)))
    
    id <- sm_id(res)
    res <- suppressWarnings(res[match(unique(id), id),])
    
    res
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

#' @rdname general_functions
#' @export
norm_diff <- function(x, y) {
    (x - y) / (x + y)
}
