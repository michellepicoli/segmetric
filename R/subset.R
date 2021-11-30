#' @title Subset handling functions
#' 
#' @name subset_handling_functions
#' 
#' @description 
#' These functions manipulate segmetric objects.
#' * `sm_list_subsets()` ...
#' * `sm_eval_subset()` ...
#' * `sm_get_subset()` ...
#' 
#' @param s      A `sf` object...
#' @param m      A `segmetric` object ...
#' @param subset A `character` value ...
#' @param expr   A `numeric` vector ...
#' 
#' @examples 
#' 
NULL

.subset_check <- function(s, allowed_types = c("ref_sf", "seg_sf", 
                                               "subset_sf")) {
    
    stopifnot(all(allowed_types %in% c("ref_sf", "seg_sf", "subset_sf")))
    
    UseMethod(".subset_check", s)
}

#' @export
.subset_check.ref_sf <- function(s) {
    
    stopifnot(inherits(s, "ref_sf"))
    stopifnot(inherits(s, "sf"))
    
    stopifnot("ref_id" %in% names(s))
    stopifnot(!"seg_id" %in% names(s))
}

#' @export
.subset_check.seg_sf <- function(s) {
    
    stopifnot(inherits(s, "seg_sf"))
    stopifnot(inherits(s, "sf"))
    
    stopifnot("seg_id" %in% names(s))
    stopifnot(!"ref_id" %in% names(s))
}

#' @export
.subset_check.subset_sf <- function(s) {
    
    stopifnot(inherits(s, "subset_sf"))
    stopifnot(inherits(s, "sf"))
    
    stopifnot("ref_id" %in% names(s))
    stopifnot("seg_id" %in% names(s))
}

#' @export
sm_list_subsets <- function(m) {
    # m checked
    
    ls(.segmetric_env(m))
}

#' @export
sm_eval_subset <- function(m, subset_id, expr = NULL) {
    # m checked
    
    if (exists(subset_id, envir = .segmetric_env(m), inherits = FALSE))
        return(m)
    
    stopifnot(!is.null(subset_id))
    
    class(subset_id) <- c("subset_sf", class(subset_id))
    .subset_check(subset_id)
    
    assign(x = subset_id, 
           value = expr, 
           envir = .segmetric_env(m), 
           inherits = FALSE)
    m
}

#' @export
sm_get_subset <- function(m, subset_id) {
    # m checked
    
    get(subset_id, envir = .segmetric_env(m), inherits = FALSE)
}

#' @export
sm_ref_id <- function(s) {
    # s checked
    
    .subset_check(s, allowed_types = "ref_sf")
    s[["ref_id"]]
}

#' @export
sm_seg_id <- function(s) {
    # s checked
    
    .subset_check(s, allowed_types = "seg_sf")
    s[["seg_id"]]
}

#' @export
sm_id <- function(s, inset = NULL) {
    # s checked
    
    if (!is.null(inset))
        .subset_check(inset, allowed_types = "subset_sf")
    
    UseMethod("sm_id", s)
}

#' @export
sm_id.ref_sf <- function(s, inset) {
    # s checked
    
    .subset_check(s, allowed_types = "ref_sf")
    
    if (!is.null(inset))
        return(inset[["ref_id"]])
    
    s[["ref_id"]]
}

#' @export
sm_id.seg_sf <- function(s, inset) {
    # s checked
    
    .subset_check(s, allowed_types = "seg_sf")
    
    if (!is.null(inset))
        return(inset[["seg_id"]])
    
    s[["seg_id"]]
}

#' @export
sm_id.subset_sf <- function(s, inset) {
    # s checked
    
    .subset_check(s, allowed_types = "subset_sf")
    
    if (!is.null(inset))
        return(paste(inset[["ref_id"]], inset[["seg_id"]], sep = ";"))
    
    paste(s[["ref_id"]], s[["seg_id"]], sep = ";")
}

