#' @title Subset handling functions
#' 
#' @name subset_handling_functions
#' 
#' @description 
#' These functions handle `subset_sf` data (inherited from `sf` class) stored in 
#' `segmetric` objects.
#' * `sm_list()` lists subsets already computed and stored in a `segmetric` 
#' object.
#' * `sm_exists()` verifies if a `subset_id` exists in a `segmetric` object.
#' * `sm_subset()` evaluates and stores a `subset_sf` object. 
#' * `sm_indirect()` finds the `subset_id` of a given `subset_sf` object stored
#' in a `segmetric` object.
#' * `sm_segmetric()` returns the `segmetric` object that stores a given
#' `subset` object (either a `ref_sf`, a `seg_sf`, or a `subset_sf`).
#' * `sm_get()` retrieves a `subset_sf` object stored in a `segmetric` object.
#' * `sm_inset()` operator equivalent to inner join but returns only objects 
#' from `x`, or its corresponding row in `y` if parameter `return_index` 
#' is `TRUE`.
#' 
#' @param s,s1,s2       Either a `ref_sf`, a `seg_sf`, or a `subset_sf` object.
#' @param m             A `segmetric` object.
#' @param subset_id     A `character` value informing a subset name.
#' @param expr          A valid piece of code in R inside curly braces. This 
#' code is evaluated to generate a subset.
#' @param return_index  A `logical` value indicating if the corresponding rows
#' in `y` should be returned instead of the actual corresponding values of `x`.
#' 
NULL

.subset_check <- function(s, 
                          allowed_types = c("ref_sf", "seg_sf", "subset_sf")) {
    
    stopifnot(all(allowed_types %in% c("ref_sf", "seg_sf", "subset_sf")))
    
    UseMethod(".subset_check", s)
}

#' @export
.subset_check.ref_sf <- function(s, ...) {
    
    stopifnot(inherits(s, "ref_sf"))
    stopifnot(inherits(s, "sf"))
    
    stopifnot("ref_id" %in% names(s))
    stopifnot(!"seg_id" %in% names(s))
}

#' @export
.subset_check.seg_sf <- function(s, ...) {
    
    stopifnot(inherits(s, "seg_sf"))
    stopifnot(inherits(s, "sf"))
    
    stopifnot("seg_id" %in% names(s))
    stopifnot(!"ref_id" %in% names(s))
}

#' @export
.subset_check.subset_sf <- function(s, ...) {
    
    stopifnot(inherits(s, "subset_sf"))
    stopifnot(inherits(s, "sf"))
    
    stopifnot("ref_id" %in% names(s))
    stopifnot("seg_id" %in% names(s))
}

#' @rdname subset_handling_functions
#' @export
sm_list <- function(m) {
    
    .segmetric_check(m)
    ls(.segmetric_env(m))
}

#' @rdname subset_handling_functions
#' @export
sm_exists <- function(m, subset_id) {
    
    .segmetric_check(m)
    exists(subset_id, envir = .segmetric_env(m), inherits = FALSE)
}

#' @rdname subset_handling_functions
#' @export
sm_subset <- function(m, subset_id, expr = NULL) {
    # m checked
    
    if (sm_exists(m, subset_id = subset_id))
        return(sm_get(m, subset_id = subset_id))
    
    stopifnot(!missing(expr))
    stopifnot(is.character(subset_id))
    
    class(expr) <- c(subset_id, "subset_sf", class(expr))
    
    attr(expr, "segmetric") <- m
    
    .subset_check(expr, allowed_types = "subset_sf")
    
    assign(x = subset_id, 
           value = expr, 
           envir = .segmetric_env(m), 
           inherits = FALSE)
    
    expr
}

#' @rdname subset_handling_functions
#' @export
sm_indirect <- function(s) {
    
    .subset_check(s)
    stopifnot(sm_exists(attr(s, "segmetric"), subset_id = class(s)[[1]]))
    class(s)[[1]]
}

#' @rdname subset_handling_functions
#' @export
sm_segmetric <- function(s) {
    
    .subset_check(s)
    m <- attr(s, "segmetric")
    .segmetric_check(m)
    m
}

#' @rdname subset_handling_functions
#' @export
sm_get <- function(m, subset_id) {
    
    get(subset_id, envir = .segmetric_env(m), inherits = FALSE)
}

#' @rdname subset_handling_functions
#' @export
sm_ref  <- function(m) {
    # checked
    
    sm_get(m, subset_id = "ref_sf")
}

#' @rdname subset_handling_functions
#' @export
sm_seg <- function(m) {
    # checked
    
    sm_get(m, subset_id = "seg_sf")
}

#' @rdname subset_handling_functions
#' @export
sm_inset <- function(s1, s2, return_index = FALSE) {
    
    UseMethod("sm_inset", s1)
}

#' @rdname subset_handling_functions
#' @export
sm_inset.ref_sf <- function(s1, s2, return_index = FALSE) {
    
    .subset_check(s1, allowed_types = "ref_sf")
    .subset_check(s2, allowed_types = "subset_sf")
    
    s1[["..#"]] <- seq_len(nrow(s1))
    s2 <- sf::st_drop_geometry(s2)["ref_id"]
    
    s1 <- dplyr::inner_join(s1, s2, by = "ref_id")
    
    if (return_index)
        return(s1[["..#"]])
    
    s1[["..#"]] <- NULL
    s1
}

#' @rdname subset_handling_functions
#' @export
sm_inset.seg_sf <- function(s1, s2, return_index = FALSE) {
    
    .subset_check(s1, allowed_types = "seg_sf")
    .subset_check(s2, allowed_types = "subset_sf")
    
    s1[["..#"]] <- seq_len(nrow(s1))
    s2 <- sf::st_drop_geometry(s2)["seg_id"]
    
    s1 <- dplyr::inner_join(s1, s2, by = "seg_id")
    
    if (return_index)
        return(s1[["..#"]])
    
    s1[["..#"]] <- NULL
    s1
}

#' @rdname subset_handling_functions
#' @export
sm_inset.subset_sf <- function(s1, s2, return_index = FALSE) {
    
    .subset_check(s1, allowed_types = "subset_sf")
    .subset_check(s2, allowed_types = "subset_sf")
    
    s1[["..#"]] <- seq_len(nrow(s1))
    s2 <- sf::st_drop_geometry(s2)[c("ref_id", "seg_id")]
    
    s1 <- dplyr::inner_join(s1, s2, by = c("ref_id", "seg_id"))
    
    if (return_index)
        return(s1[["..#"]])
    
    s1[["..#"]] <- NULL
    s1
}
