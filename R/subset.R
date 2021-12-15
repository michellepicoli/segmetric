#' @title Subset handling functions
#' 
#' @name subset_handling_functions
#' 
#' @description 
#' These functions handles `subset_sf` objects stored in `segmetric` objects.
#' * `sm_list()` lists subsets already computed.
#' * `sm_exists()` verifies if a subset_id exists in a `segmetric` object.
#' * `sm_subset()` evaluates and stores a `subset_sf` object. 
#' * `sm_indirect()` finds the subset_id of a given `subset_sf` object stored
#' in a `segmetric` object.
#' * `sm_segmetric()` returns the `segmetric` object that stores a given
#' `subset` object (either a `ref_sf`, a `seg_sf`, or a `subset_sf`).
#' * `sm_get()` retrieves a `subset_sf` object stored in a `segmetric` object.
#' * `sm_inset()` operator equivalent to inner join but returns only objects 
#' from `x`, or its corresponding row in `y` if parameter `return_index` 
#' is `TRUE`.
#' 
#' @param s         Either a `ref_sf`, a `seg_sf`, or a `subset_sf` object
#' (extension of `sf` class).
#' @param allowed_types A `character` vector containing one or more of 
#' following values: "ref_sf", "seg_sf", "subset_sf".
#' @param ...       Additional parameter (not implemented).
#' @param m         A `segmetric` object.
#' @param subset_id A `character` value informing a subset name.
#' @param expr      A valid piece of code in R inside curly braces. This 
#' code is evaluated to generate a subset, which must be provided in the 
#' last line.
#' @param x         Either a `ref_sf`, a `seg_sf`, or a `subset_sf` object
#' (extension of `sf` class).
#' @param y         A `subset_sf` object.
#' @param return_index A `logical` value indicating if the corresponding rows
#' in `y` should be returned instead of actual corresponding values of `x`.
#' 
NULL

#' @rdname subset_handling_functions
.subset_check <- function(s, allowed_types = c("ref_sf", "seg_sf", 
                                               "subset_sf")) {
    
    stopifnot(all(allowed_types %in% c("ref_sf", "seg_sf", "subset_sf")))
    
    UseMethod(".subset_check", s)
}

#' @rdname subset_handling_functions
#' @export
.subset_check.ref_sf <- function(s, ...) {
    
    stopifnot(inherits(s, "ref_sf"))
    stopifnot(inherits(s, "sf"))
    
    stopifnot("ref_id" %in% names(s))
    stopifnot(!"seg_id" %in% names(s))
}

#' @rdname subset_handling_functions
#' @export
.subset_check.seg_sf <- function(s, ...) {
    
    stopifnot(inherits(s, "seg_sf"))
    stopifnot(inherits(s, "sf"))
    
    stopifnot("seg_id" %in% names(s))
    stopifnot(!"ref_id" %in% names(s))
}

#' @rdname subset_handling_functions
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
sm_inset <- function(x, y, return_index = FALSE) {
    
    UseMethod("sm_inset", x)
}

#' @rdname subset_handling_functions
#' @export
sm_inset.ref_sf <- function(x, y, return_index = FALSE) {
    
    .subset_check(x, allowed_types = "ref_sf")
    .subset_check(y, allowed_types = "subset_sf")
    
    x[["..#"]] <- seq_len(nrow(x))
    y <- sf::st_drop_geometry(y)["ref_id"]
    
    x <- dplyr::inner_join(x, y, by = "ref_id")
    
    if (return_index)
        return(x[["..#"]])
    
    x[["..#"]] <- NULL
    x
}

#' @rdname subset_handling_functions
#' @export
sm_inset.seg_sf <- function(x, y, return_index = FALSE) {
    
    .subset_check(x, allowed_types = "seg_sf")
    .subset_check(y, allowed_types = "subset_sf")
    
    x[["..#"]] <- seq_len(nrow(x))
    y <- sf::st_drop_geometry(y)["seg_id"]
    
    x <- dplyr::inner_join(x, y, by = "seg_id")
    
    if (return_index)
        return(x[["..#"]])
    
    x[["..#"]] <- NULL
    x
}

#' @rdname subset_handling_functions
#' @export
sm_inset.subset_sf <- function(x, y, return_index = FALSE) {
    
    .subset_check(x, allowed_types = "subset_sf")
    .subset_check(y, allowed_types = "subset_sf")
    
    x[["..#"]] <- seq_len(nrow(x))
    y <- sf::st_drop_geometry(y)[c("ref_id", "seg_id")]
    
    x <- dplyr::inner_join(x, y, by = c("ref_id", "seg_id"))
    
    if (return_index)
        return(x[["..#"]])
    
    x[["..#"]] <- NULL
    x
}
