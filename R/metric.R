#' @title General functions
#' 
#' @name segmetric_functions
#' 
#' @description 
#' These functions manipulate `segmetric` objects.
#' * `sm_read()` Load the reference and segmentation polygons into segmetric.
#' * `sm_clear()` Remove the already calculated metrics from segmetric.
#' * `print()` Print a segmetric object.
#' * `plot()` Plot the reference and segmentation polygons.
#' * `summary()` Compute a measure of central tendency over the values of a metric.
#' * `sm_is_empty()` Check if a `segmetric` object is empty.
#' 
#' @param m A `segmetric` object.
#' @param object A `segmetric` object.
#' @param metric_id A character. The name of a metric.
#' @param ref_sf A `sf` object. The reference polygons.
#' @param seg_sf A `sf` object. The segmentation polygons.
#' @param ...    Additional parameters (Not implemented).
#' 
#' @seealso `sm_compute()`
#' 
#' @examples
#' # load sample datasets
#' data("sample_ref_sf", package = "segmetric")
#' data("sample_seg_sf", package = "segmetric")
#' 
#' # create segmetric object
#' m <- sm_read(ref_sf = sample_ref_sf, seg_sf = sample_seg_sf)
#' 
#' # plot geometries
#' plot(m)
#' 
#' # compute a metric
#' sm_compute(m, "AFI")
#' 
#' # summarize the metric using mean
#' sm_compute(m, "AFI") %>% summary()
#' 
#' # clear computed subsets
#' sm_clear(m)
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
    stopifnot(all(c("ref_sf", "seg_sf") %in% ls(.segmetric_env(m))))
    if (length(m) > 0) {
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

    if (is.character(ref_sf))
        ref_sf <- sf::read_sf(ref_sf)
    stopifnot(inherits(ref_sf, "sf"))
    
    if (is.character(seg_sf))
        seg_sf <- sf::read_sf(seg_sf)
    stopifnot(inherits(seg_sf, "sf"))
    
    stopifnot(sf::st_crs(ref_sf) == sf::st_crs(seg_sf))
    
    ref_sf <- suppressWarnings(
        sf::st_sf(ref_id = seq_len(nrow(ref_sf)),
                  geometry = sf::st_geometry(ref_sf),
                  sf_column_name = "geometry")
    )
        
    seg_sf <- suppressWarnings(
        sf::st_sf(seg_id = seq_len(nrow(seg_sf)), 
                  geometry = sf::st_geometry(seg_sf),
                  sf_column_name = "geometry")
    )

    class(ref_sf) <- unique(c("ref_sf", class(ref_sf)))
    class(seg_sf) <- unique(c("seg_sf", class(seg_sf)))
    
    .env <- environment()
    
    structure(list(),
              .env = .env,
              class = c("segmetric"))
}

#' @export
#' @rdname segmetric_functions
sm_clear <- function(m) {

    subsets <- sm_list(m)
    subsets <- subsets[!subsets %in% c("ref_sf", "seg_sf")]
    rm(list = subsets, envir = .segmetric_env(m), inherits = FALSE)
    m
}

#' @exportS3Method
print.segmetric <- function(x, ...) {
    print(c(x))
}

#' @exportS3Method
plot.segmetric <- function(x, ...) {
    
    ref_sf <- dplyr::transmute(sm_ref(x), type = "reference")
    seg_sf <- dplyr::transmute(sm_seg(x), type = "segmentation")
    
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
summary.segmetric <- function(object, ...) {
    
    stopifnot(inherits(object, "segmetric"))
    
    lapply(object, mean)
}

#' @export
#' @rdname segmetric_functions
sm_is_empty <- function(m) {
    
    return((length(m[[1]]) == 1 && (is.na(m[[1]]) || is.nan(m[[1]]))) || 
               is.null(m[[1]]) || 
               length(m[[1]]) == 0)
}
