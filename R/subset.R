#' @title Subset handling functions
#' 
#' @name subset_handling_functions
#' 
#' @description 
#' These functions handles `subset_sf` objects stored in `segmetric` objects.
#' * `sm_list()` lists subsets already computed.
#' * `sm_eval()` evaluates and stores a `subset_sf` object. 
#' * `sm_get()` get a `subset_sf` object already stored.
#' * `sm_id()` returns identification values of data objects 
#' (`ref_sf`, `seg_sf`, `subset_sf`).
#' 
#' @param s         Either a `ref_sf`, a `seg_sf`, or a `subset_sf` object
#' (extension of `sf` class).
#' @param allowed_types A `character` vector containing one or more of 
#' following values: "ref_sf", "seg_sf", "subset_sf".
#' @param ...       Additional parameter (not implemented).
#' @param m         A `segmetric` object.
#' @param subset_id A `character` value informing a subset name.
#' @param expr      A `subset_sf` object (an extension of `sf` class).
#' @param inset     A `subset_sf` object (an extension of `sf` class).
#' 
#' @examples 
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
    
    ls(.segmetric_env(m))
}

#' @rdname subset_handling_functions
#' @export
sm_eval <- function(m, subset_id, expr = NULL) {
    
    if (exists(subset_id, envir = .segmetric_env(m), inherits = FALSE))
        return(sm_get(m, subset_id = subset_id))
    
    stopifnot(is.character(subset_id))
    
    class(expr) <- c("subset_sf", class(expr))
    
    .subset_check(expr, allowed_types = "subset_sf")
    
    assign(x = subset_id, 
           value = expr, 
           envir = .segmetric_env(m), 
           inherits = FALSE)
    
    expr
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
    
    sm_get(
        m = m,
        subset_id = "ref_sf"
    )
}

#' @rdname subset_handling_functions
#' @export
sm_seg <- function(m) {
    # checked
    
    sm_get(
        m = m, 
        subset_id = "seg_sf"
    )
}

#' @rdname subset_handling_functions
#' @export
sm_id <- function(s, inset = NULL) {

    if (!is.null(inset))
        .subset_check(inset, allowed_types = "subset_sf")
    
    UseMethod("sm_id", s)
}

#' @rdname subset_handling_functions
#' @export
sm_id.ref_sf <- function(s, inset = NULL) {
    
    .subset_check(s, allowed_types = "ref_sf")
    
    if (!is.null(inset)) {
        .subset_check(inset, allowed_types = "subset_sf")
        return(inset[["ref_id"]][inset[["ref_id"]] %in% s[["ref_id"]]])
    }
    s[["ref_id"]]
}

#' @rdname subset_handling_functions
#' @export
sm_id.seg_sf <- function(s, inset = NULL) {
    
    .subset_check(s, allowed_types = "seg_sf")
    
    if (!is.null(inset)) {
        .subset_check(inset, allowed_types = "subset_sf")
        return(inset[["seg_id"]][inset[["seg_id"]] %in% s[["seg_id"]]])
    }
    
    s[["seg_id"]]
}

#' @rdname subset_handling_functions
#' @export
sm_id.subset_sf <- function(s, inset = NULL) {
    
    .subset_check(s, allowed_types = "subset_sf")
    
    id <- paste(s[["ref_id"]], s[["seg_id"]], sep = ";")
    
    if (!is.null(inset)) {
        .subset_check(inset, allowed_types = "subset_sf")
        inset_id <- sm_id(inset)
        return(inset_id[inset_id %in% id])
    }

    id
}

