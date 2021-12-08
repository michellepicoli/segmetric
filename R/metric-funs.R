#' @title Metric functions
#' 
#' @name metric_functions
#' 
#' @param m A `segmetric` object.
#' @param metric_id A character value with the name of a metric to be computed.
#' @param ... Any additional argument to compute
#' a metric (see details).
#' 
#' @references 
#' A complete list of cited references is available at `?segmetric`.
#' 
#' @examples 
#' # load sample datasets
#' data("sample_ref_sf", package = "segmetric")
#' data("sample_seg_sf", package = "segmetric")
#' 
#' # create segmetric object
#' m <- sm_read(ref_sf = sample_ref_sf, seg_sf = sample_seg_sf)
#' 
#' # compute AFI metric and summarize it
#' sm_compute(m, "AFI") %>% summary()
#' 
#' # compute OS1, F_measure, and US2 metrics
#' m <- sm_compute(m, "OS1") %>%
#'   sm_compute("F_measure") %>%
#'   sm_compute("US2")
#' 
#' # summarize them
#' summary(m)
#' 
NULL

#' @rdname metric_functions
#' 
#' @description 
#' 
#' `sm_compute()` is used to compute a given metric (`metric_id` parameter) 
#' from `segmetric` objects. It compares the reference to the segmentation 
#' polygons using a metric. 
#' 
#' A list with all supported metrics can be obtained 
#' by `sm_list_metrics()` (see details for more information).
#' 
#' @seealso `sm_list_metrics()`
#' 
#' @export
sm_compute <- function(m, metric_id, ...) {
    # checked
    
    .segmetric_check(m)
    
    f <- .db_get(key = metric_id)
    metrics <- names(m)
    parameters <- list(...)
    m[[length(m) + 1]] <- do.call(f[["fn"]], args = c(list(m = m), parameters))
    names(m) <- c(metrics, metric_id)
    
    m
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
OS1 <- function(m) {
    sm_norm_frac(sm_area(sm_ystar(m)), 
                 sm_area(sm_ref(m), order = sm_ystar(m)))
}

#' @rdname metric_functions
#' 
#' @aliases US1
#' 
#' @usage sm_compute(m, metric_id = "US1")
#' 
#' @details
#' `"US1"` refers to Under-segmentation. Values range .....
#' (Costa et al., 2010)
#' 
US1 <- function(m) {
    sm_norm_frac(sm_area(sm_ystar(m)), 
                 sm_area(sm_seg(m), order = sm_ystar(m)))
}
######################################
#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
OS2 <- function(m) {
    sm_norm_frac(sm_area(sm_yprime(m)), 
                 sm_area(sm_ref(m), order = sm_yprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
US2 <- function(m) {
    sm_norm_frac(sm_area(sm_yprime(m)), 
                 sm_area(sm_seg(m), order = sm_yprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
OS3 <- function(m) {
    sm_norm_frac(sm_area(sm_ycd(m)), 
                 sm_area(sm_ref(m), order = sm_ycd(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
US3 <- function(m) {
    sm_norm_frac(sm_area(sm_ycd(m)),
                 sm_area(sm_seg(m), order = sm_ycd(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
AFI <- function(m) {
    sm_norm_left(sm_area(sm_ref(m), order = sm_yprime(m)),
                 sm_area(sm_seg(m), order = sm_yprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
QR <- function(m) {
    sm_norm_frac(sm_area(sm_ystar(m)), 
                 sm_area(sm_union(sm_ref(m), 
                                  sm_seg(m), 
                                  order = sm_ystar(m))))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
D_index <- function(m) {
    sqrt((
        sm_norm_frac(sm_area(sm_ystar(m)),
                     sm_area(sm_ref(m), order = sm_ystar(m))) ^ 2 +
            sm_norm_frac(sm_area(sm_ystar(m)),
                         sm_area(sm_seg(m), order = sm_ystar(m))) ^ 2) / 2)
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
precision <- function(m) {
    sum(sm_area(sm_xprime(m))) / sum(sm_area(sm_seg(m), order = sm_xprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
recall <- function(m) {
    sum(sm_area(sm_yprime(m))) / sum(sm_area(sm_ref(m), order = sm_yprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
UMerging <- function(m) {
    sm_norm_left(sm_area(sm_ref(m), order = sm_ystar(m)), sm_area(sm_ystar(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
OMerging <- function(m) {
    (sm_area(sm_seg(m), order = sm_ystar(m)) - sm_area(sm_ystar(m))) /
        sm_area(sm_ref(m), order = sm_ystar(m))
    
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
M <- function(m) {
    sm_area(sm_yprime(m)) / 
        sqrt(sm_area(sm_ref(m), order = sm_yprime(m)) *
                 sm_area(sm_seg(m), order = sm_yprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
E <- function(m) {
    # TODO: check formula in Carleer et al. (2005)
    sm_norm_left(sm_area(sm_seg(m), order = sm_xprime(m)),
                 sm_area(sm_xprime(m))) * 100
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
RAsub <- function(m) {
    sm_area(sm_ytilde(m)) / sm_area(sm_ref(m), order = sm_ytilde(m))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
RAsuper <- function(m) {
    sm_area(sm_ytilde(m)) / sm_area(sm_seg(m), order = sm_ytilde(m))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
PI <- function(m) {
    # TODO: check formula
    sm_area(sm_ytilde(m)) ^ 2 / (
        sm_area(sm_ref(m), order = sm_ytilde(m)) *
            sm_area(sm_seg(m), order = sm_ytilde(m))
    )
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
Fitness <- function(m) {
    (sm_area(sm_seg(m), order = sm_xprime(m)) +
         sm_area(sm_ref(m), order = sm_xprime(m)) -
         2 * sm_area(sm_xprime(m))) /
        sm_area(sm_seg(m), order = sm_xprime(m))
}

#' @rdname metric_functions
#' 
#' @aliases OS1
#' 
#' @usage sm_compute(m, metric_id = "OS1")
#' 
#' @details
#' `OS1`: refers to Over-segmentation. Values range .....
#' 
ED3 <- function(m) {
    sqrt((sm_norm_frac(sm_area(sm_ycd(m)),
                       sm_area(sm_ref(m), order = sm_ycd(m))) ^ 2 +
              sm_norm_frac(sm_area(sm_ycd(m)),
                           sm_area(sm_seg(m), order = sm_ycd(m))) ^ 2) / 2)
}
################################
#' @rdname metric_functions
#' 
#' @aliases F_measure
#' 
#' @usage sm_compute(m, metric_id = "F_measure", alpha = 0.5)
#' 
#' @details
#' `"F_measure"`: refers to F-measure metric (Van Rijsbergen, 1979; 
#' Zhang et al., 2015). Values range from 0 to 1 (optimal). It takes the 
#' optional weight argument `alpha`, which ranges from `0.0` to `1.0` 
#' (default is `0.5`).
#' 
F_measure <- function(m, alpha = 0.5) {
    stopifnot(alpha >= 0)
    stopifnot(alpha <= 1)
    1 / ((alpha / (sum(sm_area(sm_xprime(m))) / 
                       sum(sm_area(sm_seg(m), order = sm_xprime(m))))) + 
             ((1 - alpha) / 
                  (sum(sm_area(sm_yprime(m))) / 
                       sum(sm_area(sm_ref(m), order = sm_yprime(m))))))
}
