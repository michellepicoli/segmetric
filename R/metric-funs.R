#' @title Metric functions
#' 
#' @name metric_functions
#' 
#' @param m A `segmetric` object.
#' @param metric_id  A `character` vector with metrics id to be computed.
#' @param ... Any additional argument to compute
#' a metric (see Details).
#' 
#' @references 
#' A complete list of cited references is available in `?segmetric`.
#' 
#' 
#' @details
#' - "`OS1`" refers to Oversegmentation. Its values range from 0 (optimal) to 1
#' (Clinton et al., 2010).
#' - "`US1`" refers to Undersegmentation. Its values range from 0 (optimal) to 1 
#' (Clinton et al., 2010).
#' - "`OS2`" refers to Oversegmentation. Its values range from 0 (optimal) to 1 
#' (Persello and Bruzzone, 2010).
#' - "`US2`" refers to Undersegmentation. Its values range from 0 (optimal) to 1 
#' (Persello and Bruzzone, 2010).
#' - "`OS3`" refers to Oversegmentation. Its values range from 0 (optimal) to 1 
#' (Yang et al., 2014).
#' - "`US3`" refers to Undersegmentation. Its values range from 0 (optimal) to 1 
#' (Yang et al., 2014).
#' - "`AFI`" refers to Area Fit Index. Its optimal value is 0 (Lucieer and Stein, 
#' 2002; Clinton et al., 2010).
#' - "`QR`" refers to Quality Rate. Its values range from 0 (optimal) to 1 
#' (Weidner, 2008; Clinton et al., 2010).
#' - "`D_index`" refers to Index D. Its values range from 0 (optimal) to 1 
#' (Levine and Nazif, 1982; Clinton et al., 2010).
#' - "`precision`" refers to Precision. Its values range from 0 to 1 (optimal) 
#' (Van Rijsbergen, 1979; Zhang et al., 2015).
#' - "`recall`" refers to Recall. Its values range from 0 to 1 (optimal) (Van 
#' Rijsbergen, 1979; Zhang et al., 2015).
#' - "`UMerging`" refers to Undermerging. Its values range from 0 (optimal) to 1 
#' (Levine and Nazif, 1982; Clinton et al., 2010).
#' - "`OMerging`" refers to Overmerging. Its optimal value is 0 
#' (Levine and Nazif, 1982; Clinton et al., 2010).
#' - "`M`" refers to Match. Its values range from 0 to 1 (optimal) (Janssen and 
#' Molenaar, 1995; Feitosa et al., 2010).
#' - "`E`" refers to Evaluation Measure. Its values range from 0 (optimal) to 100 
#' (Carleer et al., 2005).
#' - "`RAsub`" refers to Relative Area. Its values range from 0 to 1 (optimal) 
#' (Müller et al., 2007; Clinton et al., 2010).
#' - "`RAsuper`" refers to Relative area. Its values range from 0 to 1 (optimal) 
#' (Müller et al., 2007; Clinton et al., 2010).
#' - "`PI`" refers to Purity Index. Its values range from 0 to 1 (optimal) (van 
#' Coillie et al., 2008).
#' - "`Fitness`" refers to Fitness Function. Its optimal value is 0 (Costa et al., 
#' 2008).
#' - "`ED3`" refers to Euclidean Distance. Its values range from 0 (optimal) to 1 
#' (Yang et al., 2014).
#' - "`F_measure`" refers to F-measure metric. Its values range from 0 to 1 
#' (optimal) (Van Rijsbergen, 1979; Zhang et al., 2015). It takes the optional 
#' weight argument `alpha`, ranging from `0.0` to `1.0` (the default is `0.5`).
#' 
#' @return Return a `numeric` vector with computed metric.
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
#' # compute three metrics and summarize them
#' sm_compute(m, c("AFI", "OS1", "US2")) %>% summary()
#' 
#' # compute OS1, F_measure, and US2 metrics using pipe
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
#' The `sm_compute()` computes a given metric (`metric_id` parameter) from 
#' segmentation objects. It compares the reference to the segmentation 
#' polygons using a metric. 
#' 
#' A list with all supported metrics can be obtained 
#' by `sm_list_metrics()` (see Details for more information).
#' 
#' @seealso `sm_list_metrics()`
#' 
#' @export
sm_compute <- function(m, metric_id, ...) {
    
    .segmetric_check(m)
    
    parameters <- list(...)
    
    for (i in seq_along(metric_id)) {
        metrics <- names(m)
        f <- .db_get(key = metric_id[[i]])
        m[[length(m) + 1]] <- do.call(f[["fn"]], 
                                      args = c(list(m = m), parameters))
        names(m) <- c(metrics, metric_id[[i]])
    }
    
    m
}

#' @rdname metric_functions
#' @description 
#' 
#' The `sm_metric_subset()` returns the subset used to compute the metrics
#' in segmetric object. 
#' 
#' @export
sm_metric_subset <- function(m, metric_id = NULL) {
    
    .segmetric_check(m)
   
    if (!is.null(metric_id))
        m <- m[metric_id]
    
    result <- list()
    metrics <- names(m)
    for (i in seq_along(m)) {
        f <- .db_get(key = metrics[[i]])
        if (!is.null(f[["fn_subset"]])) {
            result[[i]] <- do.call(f[["fn_subset"]], args = list(m = m))
            result[[i]][metrics[[i]]] <- m[[metrics[[i]]]]
        } else 
            result[[i]] <- NULL
    }
    names(result) <- metrics
    
    result
}

OS1 <- function(m, ...) {
    .norm_right(sm_area(sm_ystar(m)), 
                 sm_area(sm_ref(m), order = sm_ystar(m)))
}

US1 <- function(m, ...) {
    .norm_right(sm_area(sm_ystar(m)), 
                 sm_area(sm_seg(m), order = sm_ystar(m)))
}

OS2 <- function(m, ...) {
    .norm_right(sm_area(sm_yprime(m)), 
                 sm_area(sm_ref(m), order = sm_yprime(m)))
}

US2 <- function(m, ...) {
    .norm_right(sm_area(sm_yprime(m)), 
                 sm_area(sm_seg(m), order = sm_yprime(m)))
}

OS3 <- function(m, ...) {
    .norm_right(sm_area(sm_ycd(m)), 
                 sm_area(sm_ref(m), order = sm_ycd(m)))
}

US3 <- function(m, ...) {
    .norm_right(sm_area(sm_ycd(m)),
                 sm_area(sm_seg(m), order = sm_ycd(m)))
}

AFI <- function(m, ...) {
    .norm_left(sm_area(sm_ref(m), order = sm_yprime(m)),
                 sm_area(sm_seg(m), order = sm_yprime(m)))
}

QR <- function(m, ...) {
    .norm_right(sm_area(sm_ystar(m)), 
                 sm_area(sm_subset_union(sm_ystar(m))))
}

D_index <- function(m, ...) {
    sqrt((
        .norm_right(sm_area(sm_ystar(m)),
                     sm_area(sm_ref(m), order = sm_ystar(m))) ^ 2 +
            .norm_right(sm_area(sm_ystar(m)),
                         sm_area(sm_seg(m), order = sm_ystar(m))) ^ 2) / 2)
}

precision <- function(m, ...) {
    sum(sm_area(sm_xprime(m))) / sum(sm_area(sm_seg(m), order = sm_xprime(m)))
}

recall <- function(m, ...) {
    sum(sm_area(sm_yprime(m))) / sum(sm_area(sm_ref(m), order = sm_yprime(m)))
}

UMerging <- function(m, ...) {
    .norm_left(sm_area(sm_ref(m), order = sm_ystar(m)), sm_area(sm_ystar(m)))
}

OMerging <- function(m, ...) {
    (sm_area(sm_seg(m), order = sm_ystar(m)) - sm_area(sm_ystar(m))) /
        sm_area(sm_ref(m), order = sm_ystar(m))
    
}

M <- function(m, ...) {
    sm_area(sm_yprime(m)) / 
        sqrt(sm_area(sm_ref(m), order = sm_yprime(m)) *
                 sm_area(sm_seg(m), order = sm_yprime(m)))
}

E <- function(m, ...) {
    # TODO: check formula in Carleer et al. (2005)
    .norm_left(sm_area(sm_seg(m), order = sm_xprime(m)),
                 sm_area(sm_xprime(m))) * 100
}

RAsub <- function(m, ...) {
    sm_area(sm_ytilde(m)) / sm_area(sm_ref(m), order = sm_ytilde(m))
}

RAsuper <- function(m, ...) {
    sm_area(sm_ytilde(m)) / sm_area(sm_seg(m), order = sm_ytilde(m))
}

PI <- function(m, ...) {
    sm_area(sm_ytilde(m)) ^ 2 / (
        sm_area(sm_ref(m), order = sm_ytilde(m)) *
            sm_area(sm_seg(m), order = sm_ytilde(m))
    )
}

Fitness <- function(m, ...) {
    (sm_area(sm_seg(m), order = sm_xprime(m)) +
         sm_area(sm_ref(m), order = sm_xprime(m)) -
         2 * sm_area(sm_xprime(m))) /
        sm_area(sm_seg(m), order = sm_xprime(m))
}

ED3 <- function(m, ...) {
    sqrt(OS3(m) ^ 2 + US3(m) ^ 2) / 2
}

F_measure <- function(m, ..., alpha = 0.5) {
    stopifnot(alpha >= 0)
    stopifnot(alpha <= 1)
    1 / ((alpha / (sum(sm_area(sm_xprime(m))) / 
                       sum(sm_area(sm_seg(m), order = sm_xprime(m))))) + 
             ((1 - alpha) / 
                  (sum(sm_area(sm_yprime(m))) / 
                       sum(sm_area(sm_ref(m), order = sm_yprime(m))))))
}
