#' @title General functions
#' 
#' @name segmetric_functions
#' 
#' @description 
#' These functions manipulate `segmetric` objects.
#' * `sm_read()` Load the reference and segmentation polygons into segmetric.
#' * `sm_clear()` Remove the already calculated metrics from segmetric.
#' * `sm_compute()` Compare the reference to the segmentation polygons using a metric.
#' * `plot()` Plot the reference and segmentation polygons.
#' * `summary()` Compute a measure of central tendency over the values of a metric.
#' * `sm_ref_area()` Return the areas of the reference polygons.
#' * `sm_seg_area()` Return the areas of the segmentation polygons.
#' * `sm_inter_area()` Return the areas of the intersection between the reference and segmentation polygons.
#' * `sm_is_empty()` Check if a `segmetric` object is empty.
#' 
#' @param m A `segmetric` object.
#' @param metric_id A character. The name of a metric.
#' @param ref_sf A `sf` object. The reference polygons.
#' @param seg_sf A `sf` object. The segmentation polygons.
#' @param weight A `numeric` vector ...
#' @param ...    Additional parameters. See details.
#' 
#' @details
#' \itemize{
#' \item{\code{F_measure} takes the optional weight argument `alpha`, which by 
#' default is `0.5`. For more information check `sm_desc_metric("F_measure")`.
#' } 
#' } 
#' 
#' @examples 
#' 
NULL


#' @title Internal function
#' 
#' @description
#' This function check if a segmetric object is valid.
#' 
#' @keywords internal
#' 
.segmetric_check <- function(m) {
    
    stopifnot(inherits(m, "segmetric"))
    stopifnot(length(m) <= 1)
    stopifnot(all(c("ref_sf", "seg_sf") %in% sm_list(m)))
    if (length(m) == 1) {
        stopifnot(!is.null(names(m)))
        stopifnot(names(m) %in% .db_list())
    }
}

#' @title Internal function
#' 
#' @description
#' This function returns the segmetric environment. 
#' 
#' @keywords internal
#' 
.segmetric_env <- function(m) {
    
    attr(m, which = ".env", exact = TRUE)
}

#' @export
#' @rdname segmetric_functions
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
    
    structure(list(),
              .env = .env,
              class = c("segmetric"))
}

#' @export
#' @rdname segmetric_functions
sm_clear <- function(m) {
    # checked
    
    subsets <- sm_list(m)
    subsets <- subsets[!subsets %in% c("ref_sf", "seg_sf")]
    rm(list = subsets, envir = .segmetric_env(m), inherits = FALSE)
    m
}

#' @export
#' @rdname segmetric_functions
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
    
    ref_sf <- dplyr::transmute(sm_ref(m), type = "reference")
    seg_sf <- dplyr::transmute(sm_seg(m), type = "segmentation")
    
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
#' @rdname segmetric_functions
summary.segmetric <- function(m, weight = NULL, ...) {
    
    stopifnot(inherits(m, "segmetric"))
    
    if (!is.null(weight))
        return(lapply(m, weighted.mean, w = weight))
    
    lapply(m, mean)
}

#' @export
sm_ref_area <- function(m) {
    
    sm_check(m = m)
    
    f <- .db_get(key = names(m))
    ref_sf <- sm_get(m = m, subset_id = "ref_sf")
    ordering <- sm_get(m = m, subset_id = f[["depends"]][[1]])
    
    sm_area(ref_sf, order = ordering)
}

#' @export
sm_seg_area <- function(m) {
    sm_check(m = m)
    
    f <- .db_get(key = names(m))
    seg_sf <- sm_get(m = m, subset_id = "seg_sf")
    ordering <- sm_get(m = m, subset_id = f[["depends"]][[1]])
    
    sm_area(seg_sf, order = ordering)
}

#' @export
sm_inter_area <- function(m) {
    sm_check(m = m)
    
    f <- .db_get(key = names(m))
    
    sm_area(sm_get(m = m, subset_id = f[["depends"]][[1]]))
}

#' @export
sm_is_empty <- function(m) {
    
    return((length(m[[1]]) == 1 && is.na(m[[1]])) || 
               is.null(m[[1]]) || 
               length(m[[1]]) == 0)
}
