#' @title General functions
#' 
#' @name general_functions
#' 
#' @description 
#' These functions manipulate segmetric objects.
#' * `sm_read()` ...
#' * `sm_clear()` ...
#' * `sm_compute()` ...
#' * `summary.segmetric()` ...
#' * `get_ref_area()` ...
#' * `get_seg_area()` ...
#' * `get_inter_area()` ...
#' 
#' @param ref_sf A `sf` object...
#' @param seg_sf A `sf` object...
#' @param m      A `segmetric` object ...
#' @param metric A `character` value ...
#' @param ...    Additional parameters. See details.
#' @param weight A `numeric` vector ...
#' 
#' @examples 
#' 
NULL

.segmetric_check <- function(m) {
    
    stopifnot(inherits(m, "segmetric"))
    stopifnot(length(m) <= 1)
    stopifnot(all(c("ref_sf", "seg_sf") %in% sm_list_subsets(m)))
    if (length(m) == 1) {
        stopifnot(!is.null(names(m)))
        stopifnot(names(m) %in% .db_list())
    }
}

.segmetric_env <- function(m) {
    
    attr(m, which = ".env", exact = TRUE)
}

#' @export
#' @rdname general_functions
sm_read <- function(ref_sf, seg_sf) {
    # checked
    
    if (is.character(ref_sf))
        ref_sf <- sf::read_sf(ref_sf)
    stopifnot(inherits(ref_sf, "sf"))
    
    if (is.character(seg_sf))
        seg_sf <- sf::read_sf(seg_sf)
    stopifnot(inherits(seg_sf, "sf"))
    
    stopifnot(sf::st_crs(ref_sf) == sf::st_crs(seg_sf))
    
    ref_sf[["ref_id"]] <- seq_len(nrow(ref_sf))
    seg_sf[["seg_id"]] <- seq_len(nrow(seg_sf))
    
    class(ref_sf) <- c("ref_sf", class(ref_sf))
    class(seg_sf) <- c("seg_sf", class(seg_sf))
    
    .env <- environment()
    
    m <- structure(list(),
                   .env = .env,
                   class = c("segmetric"))
    
    .segmetric_check(m)
    m
}

#' @export
#' @rdname general_functions
sm_clear <- function(m) {
    # checked
    
    subsets <- sm_list_subsets(m)
    subsets <- subsets[!subsets %in% c("ref_sf", "seg_sf")]
    rm(list = subsets, envir = .segmetric_env(m), inherits = FALSE)
    m
}

#' @export
#' @rdname general_functions
sm_compute <- function(m, metric_id, ...) {
    # checked
    
    .segmetric_check(m)
    
    f <- .db_get(key = metric_id)
    
    parameters <- list(...)
    m[[1]] <- do.call(f[["fn"]], args = c(list(m = m), parameters))
    names(m) <- metric_id
    
    m
}

#' @exportS3Method
plot.segmetric <- function(m, ...) {
    
    ref_sf <- dplyr::transmute(ref_sf(m), type = "reference")
    seg_sf <- dplyr::transmute(seg_sf(m), type = "segmentation")
    
    plot(sf::st_geometry(ref_sf),
         border = 'blue',
         extent = rbind(ref_sf, seg_sf),
         main = "Reference (blue) versus Segmentation (red) and their centroids")
    
    plot(sf::st_geometry(seg_sf),
         border = 'red',
         add = TRUE)
    
    plot(sf::st_centroid(sf::st_geometry(ref_sf)), 
         pch = 1,
         col = 'blue',
         add = TRUE)
    
    plot(sf::st_centroid(sf::st_geometry(seg_sf)), 
         pch = 2,
         col = 'red',
         add = TRUE)
}

#' @exportS3Method
#' @rdname general_functions
summary.segmetric <- function(m, weight = NULL, ...) {
    
    stopifnot(inherits(m, "segmetric"))
    
    if (!is.null(weight))
        return(lapply(m, weighted.mean, w = weight))
    
    lapply(m, mean)
}


#' @export
sm_ref  <- function(m) {
    # checked
    
    sm_get_subset(
        m = m,
        subset_id = "ref_sf"
    )
}

#' @export
sm_seg <- function(m) {
    # checked
    
    sm_get_subset(
        m = m, 
        subset_id = "seg_sf"
    )
}

#' @export
get_ref_area <- function(m) {
    
    sm_check(m = m)
    
    f <- .db_get(key = names(m))
    ordering <- f[["depends"]][[1]]
    
    ref_sf <- sm_get_subset(m = m, subset = "ref_sf")
    ref_rows <- ref_id(sm_get_subset(m = m, subset = ordering))
    
    area(ref_sf, order = ref_rows)
}

#' @export
get_seg_area <- function(m) {
    sm_check(m = m)
    
    f <- .db_get(key = names(m))
    ordering <- f[["depends"]][[1]]
    
    seg_sf <- sm_get_subset(m = m, subset = "seg_sf")
    seg_rows <- seg_id(sm_get_subset(m = m, subset = ordering))
    
    area(seg_sf, order = seg_rows)
}

#' @export
get_inter_area <- function(m) {
    sm_check(m = m)
    
    f <- .db_get(key = names(m))
    field <- f[["depends"]][[1]]
    
    area(sm_get_subset(m = m, subset = field))
}
