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

#' @param m         A `segmetric`object.
#' 
#' 
#' @details
#' `"OS1"` refers to Oversegmentation. Its values range from 0 (optimal) to 1
#' (Clinton et al., 2010).
OS1 <- function(m) {
    .norm_right(sm_area(sm_ystar(m)), 
                 sm_area(sm_ref(m), order = sm_ystar(m)))
}

#' @rdname metric_functions
#' 
#' @aliases US1
#' 
#' 
#' @details
#' `"US1"` refers to Undersegmentation. Its values range from 0 (optimal) to 1 
#' (Clinton et al., 2010).
US1 <- function(m) {
    .norm_right(sm_area(sm_ystar(m)), 
                 sm_area(sm_seg(m), order = sm_ystar(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS2
#' 
#' 
#' @details
#' `"OS2"` refers to Oversegmentation. Its values range from 0 (optimal) to 1 
#' (Persello and Bruzzone, 2010).
OS2 <- function(m) {
    .norm_right(sm_area(sm_yprime(m)), 
                 sm_area(sm_ref(m), order = sm_yprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases US2
#' 
#' 
#' @details
#' `"US2"` refers to Undersegmentation. Its values range 0 (optimal) to 1 
#' (Persello and Bruzzone, 2010).
US2 <- function(m) {
    .norm_right(sm_area(sm_yprime(m)), 
                 sm_area(sm_seg(m), order = sm_yprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OS3
#' 
#' 
#' @details
#' `"OS3"` refers to Oversegmentation. Its values range from 0 (optimal) to 1 
#' (Yang et al., 2014).
OS3 <- function(m) {
    .norm_right(sm_area(sm_ycd(m)), 
                 sm_area(sm_ref(m), order = sm_ycd(m)))
}

#' @rdname metric_functions
#' 
#' @aliases US3
#' 
#' 
#' @details
#' `"US3"` refers to Undersegmentation. Its values range from 0 (optimal) to 1 
#' (Yang et al., 2014).
US3 <- function(m) {
    .norm_right(sm_area(sm_ycd(m)),
                 sm_area(sm_seg(m), order = sm_ycd(m)))
}

#' @rdname metric_functions
#' 
#' @aliases AFI
#' 
#' 
#' @details
#' `"AFI"` refers to Area Fit Index. Its optimal value is 0 (Lucieer and Stein, 
#' 2002; Clinton et al., 2010).
AFI <- function(m) {
    .norm_left(sm_area(sm_ref(m), order = sm_yprime(m)),
                 sm_area(sm_seg(m), order = sm_yprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases QR
#' 
#' 
#' @details
#' `"QR"` refers to Quality rate. Its values range from 0 (optimal) to 1 
#' (Weidner, 2008; Clinton et al., 2010).
QR <- function(m) {
    .norm_right(sm_area(sm_ystar(m)), 
                 sm_area(sm_subset_union(sm_ystar(m))))
}

#' @rdname metric_functions
#' 
#' @aliases D_index
#' 
#' 
#' @details
#' `"D_index"` refers to Index D. Its values range from 0 (optimal) to 1 
#' (Levine and Nazif, 1982; Clinton et al., 2010).
D_index <- function(m) {
    sqrt((
        .norm_right(sm_area(sm_ystar(m)),
                     sm_area(sm_ref(m), order = sm_ystar(m))) ^ 2 +
            .norm_right(sm_area(sm_ystar(m)),
                         sm_area(sm_seg(m), order = sm_ystar(m))) ^ 2) / 2)
}

#' @rdname metric_functions
#' 
#' @aliases precision
#' 
#' 
#' @details
#' `"precision"` refers to Precision. Its values range from 0 to 1 (optimal) 
#' (Van Rijsbergen, 1979; Zhang et al., 2015).
precision <- function(m) {
    sum(sm_area(sm_xprime(m))) / sum(sm_area(sm_seg(m), order = sm_xprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases recall
#' 
#' 
#' @details
#' `"recall"` refers to Recall. Its values range from 0 to 1 (optimal) (Van 
#' Rijsbergen, 1979; Zhang et al., 2015).
recall <- function(m) {
    sum(sm_area(sm_yprime(m))) / sum(sm_area(sm_ref(m), order = sm_yprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases UMerging
#' 
#' 
#' @details
#' `"UMerging"` refers to Undermerging. Its values range from 0 (optimal) to 0.5 
#' (Levine and Nazif, 1982; Clinton et al., 2010).
UMerging <- function(m) {
    .norm_left(sm_area(sm_ref(m), order = sm_ystar(m)), sm_area(sm_ystar(m)))
}

#' @rdname metric_functions
#' 
#' @aliases OMerging
#' 
#' 
#' @details
#' `"OMerging"` refers to Overmerging. Its values range from 0 (optimal) to 0.5 
#' (Levine and Nazif, 1982; Clinton et al., 2010).
OMerging <- function(m) {
    (sm_area(sm_seg(m), order = sm_ystar(m)) - sm_area(sm_ystar(m))) /
        sm_area(sm_ref(m), order = sm_ystar(m))
    
}

#' @rdname metric_functions
#' 
#' @aliases M
#' 
#' 
#' @details
#' `"M"` refers to Match. Its values range from 0 to 1 (optimal) (Janssen and 
#' Molenaar, 1995; Feitosa et al., 2010).
M <- function(m) {
    sm_area(sm_yprime(m)) / 
        sqrt(sm_area(sm_ref(m), order = sm_yprime(m)) *
                 sm_area(sm_seg(m), order = sm_yprime(m)))
}

#' @rdname metric_functions
#' 
#' @aliases E
#' 
#' 
#' @details
#' `"E"` refers to Evaluation measure. Its values range from 0 (optimal) to 50 
#' (Carleer et al., 2005).
E <- function(m) {
    # TODO: check formula in Carleer et al. (2005)
    .norm_left(sm_area(sm_seg(m), order = sm_xprime(m)),
                 sm_area(sm_xprime(m))) * 100
}

#' @rdname metric_functions
#' 
#' @aliases RAsub
#' 
#' 
#' @details
#' `"RAsub"` refers to Relative area. Its values range from 0 to 1 (optimal) 
#' (Müller et al., 2007; Clinton et al., 2010).
RAsub <- function(m) {
    sm_area(sm_ytilde(m)) / sm_area(sm_ref(m), order = sm_ytilde(m))
}

#' @rdname metric_functions
#' 
#' @aliases RAsuper
#' 
#' 
#' @details
#' `"RAsuper"` refers to Relative area. Its values range from 0 to 1 (optimal) 
#' (Müller et al., 2007; Clinton et al., 2010).
RAsuper <- function(m) {
    sm_area(sm_ytilde(m)) / sm_area(sm_seg(m), order = sm_ytilde(m))
}

#' @rdname metric_functions
#' 
#' @aliases PI
#' 
#' 
#' @details
#' `"PI"` refers to Purity Index. Its values range from 0 to 1 (optimal) (van 
#' Coillie et al., 2008).
PI <- function(m) {
    sm_area(sm_ytilde(m)) ^ 2 / (
        sm_area(sm_ref(m), order = sm_ytilde(m)) *
            sm_area(sm_seg(m), order = sm_ytilde(m))
    )
}

#' @rdname metric_functions
#' 
#' @aliases Fitness
#' 
#' 
#' @details
#' `"Fitness"` refers to Fitness function. Its optimal value is 0 (Costa et al., 
#' 2008).
Fitness <- function(m) {
    (sm_area(sm_seg(m), order = sm_xprime(m)) +
         sm_area(sm_ref(m), order = sm_xprime(m)) -
         2 * sm_area(sm_xprime(m))) /
        sm_area(sm_seg(m), order = sm_xprime(m))
}

#' @rdname metric_functions
#' 
#' @aliases ED3
#' 
#' 
#' @details
#' `"ED3"` refers to Euclidean Distance. Its values range from 0 (optimal) to 1 
#' (Yang et al., 2014).
ED3 <- function(m) {
    sqrt((.norm_right(sm_area(sm_ycd(m)),
                       sm_area(sm_ref(m), order = sm_ycd(m))) ^ 2 +
              .norm_right(sm_area(sm_ycd(m)),
                           sm_area(sm_seg(m), order = sm_ycd(m))) ^ 2) / 2)
}

#' @rdname metric_functions
#' 
#' @aliases F_measure
#' 
#' @param alpha Numeric value ranging from 0 to 1. See Details.
#' 
#' @details
#' `"F_measure"` refers to F-measure metric. Its values range from 0 to 1 
#' (optimal) (Van Rijsbergen, 1979; Zhang et al., 2015). It takes the 
#' optional weight argument `alpha`, which ranges from `0.0` to `1.0` 
#' (the default is `0.5`).
F_measure <- function(m, alpha = 0.5) {
    stopifnot(alpha >= 0)
    stopifnot(alpha <= 1)
    1 / ((alpha / (sum(sm_area(sm_xprime(m))) / 
                       sum(sm_area(sm_seg(m), order = sm_xprime(m))))) + 
             ((1 - alpha) / 
                  (sum(sm_area(sm_yprime(m))) / 
                       sum(sm_area(sm_ref(m), order = sm_yprime(m))))))
}
