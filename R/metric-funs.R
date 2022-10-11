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
#' - "`IoU`" refers to Intersection over Union metric. Its values range 
#' from 0 to 1 (optimal) (Jaccard, 1912; Rezatofighi et al., 2019).
#' - "`SimSize`" refers to the similarity size metric. Its values range from 
#' 0 to 1 (optimal) (Zhan et al., 2005).
#' - "`qLoc`"refers to quality of object’s location metric. Its optimal value 
#' is 0 (Zhan et al., 2005).
#' - "`RPsub`" refers to Relative Position (sub) metric. Optimal value is 0 
#' (Möller et al., 2007, Clinton et al., 2010).
#' - "`RPsuper`" refers to Relative Position (super) metric. Its values range 
#' from 0 (optimal) to 1 (Möller et al., 2007, Clinton et al., 2010).
#' - "`OI2` refers to Overlap Index metric. Its values range from 0 to 1
#' (optimal) (Yang et al., 2017).
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
    
    for (metric in metric_id) {
        f <- .db_get(key = metric)
        s <- NULL
        if (!is.null(f[["fn_subset"]])) {
            s <- do.call(f[["fn_subset"]], args = c(list(m = m)))
        }
        m[[metric]] <- do.call(
            f[["fn"]], args = c(list(m = m, s = s), parameters)
        )
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

OS1 <- function(m, s, ...) {
    sm_norm_right(sm_area(s), sm_area(sm_ref(m), order = s))
}

US1 <- function(m, s, ...) {
    sm_norm_right(sm_area(s), sm_area(sm_seg(m), order = s))
}

OS2 <- function(m, s, ...) {
    sm_norm_right(sm_area(s), sm_area(sm_ref(m), order = s))
}

US2 <- function(m, s, ...) {
    sm_norm_right(sm_area(s), sm_area(sm_seg(m), order = s))
}

OS3 <- function(m, s, ...) {
    sm_norm_right(sm_area(s), sm_area(sm_ref(m), order = s))
}

US3 <- function(m, s, ...) {
    sm_norm_right(sm_area(s), sm_area(sm_seg(m), order = s))
}

AFI <- function(m, s, ...) {
    sm_norm_left(sm_area(sm_ref(m), order = s), sm_area(sm_seg(m), order = s))
}

QR <- function(m, s, ...) {
    sm_norm_right(sm_area(s), sm_area(sm_subset_union(s)))
}

D_index <- function(m, s, ...) {
    # m <- sm_compute(m, metric_id = c("OS1", "US1"))
    sqrt((OS1(m, s)^2 + US1(m, s)^2) / 2)
}

precision <- function(m, s, ...) {
    sum(sm_area(s)) / sum(sm_area(sm_seg(m), order = s))
}

recall <- function(m, s, ...) {
    sum(sm_area(s)) / sum(sm_area(sm_ref(m), order = s))
}

UMerging <- function(m, s, ...) {
    sm_norm_left(sm_area(sm_ref(m), order = s), sm_area(s))
}

OMerging <- function(m, s, ...) {
    (sm_area(sm_seg(m), order = s) - sm_area(s)) / 
        sm_area(sm_ref(m), order = s)
    
}

M <- function(m, s, ...) {
    sm_area(s) / sqrt(
        sm_area(sm_ref(m), order = s) * sm_area(sm_seg(m), order = s)
    )
}

E <- function(m, s, ...) {
    sm_norm_left(sm_area(sm_seg(m), order = s), sm_area(s)) * 100
}

RAsub <- function(m, s, ...) {
    sm_area(s) / sm_area(sm_ref(m), order = s)
}

RAsuper <- function(m, s, ...) {
    sm_area(s) / sm_area(sm_seg(m), order = s)
}

PI <- function(m, s, ...) {
    x <- sm_area(s) ^ 2 / (
        sm_area(sm_ref(m), order = s) * sm_area(sm_seg(m), order = s)
    )
    sm_apply_group(x, groups = s[["ref_id"]], sum)
}

Fitness <- function(m, s, ...) {
    (
        sm_area(sm_seg(m), order = s) + sm_area(sm_ref(m), order = s) - 
            2 * sm_area(s)
    ) / sm_area(sm_seg(m), order = s)
}

ED3 <- function(m, s, ...) {
    sqrt((OS3(m, s)^2 + US3(m, s)^2) / 2)
}

F_measure <- function(m, ..., alpha = 0.5) {
    stopifnot(alpha >= 0)
    stopifnot(alpha <= 1)
    
    1 / ((alpha / sm_compute(m, "precision")[["precision"]]) + 
             ((1 - alpha) / sm_compute(m, "recall")[["recall"]]))
}

IoU <- function(m, s, ...) {
    sm_area(s) / sm_area(sm_subset_union(s))
}

SimSize <- function(m, s, ...) {
    pmin(sm_area(sm_ref(m), order = s), sm_area(sm_seg(m), order = s)) /
        pmax(sm_area(sm_ref(m), order = s), sm_area(sm_seg(m), order = s))
}

qLoc <- function(m, s, ...) {
    sm_distance(
        sm_centroid(sm_ref(m), order = s), sm_centroid(sm_seg(m), order = s)
    )
}

RPsub <- function(m, s, ...) {
    sm_distance(
        sm_centroid(sm_ref(m), order = s), sm_centroid(sm_seg(m), order = s)
    )
}

RPsuper <- function(m, s, ...) {
    sm_apply_group(
        x = qLoc(m, s), groups = s[["ref_id"]], fn = function(x) {
            if (length(x) == 1 && x == 0)
                return(0)
            x / max(x)
        }
    )
}

OI2 <- function(m, s, ...) {
    sm_apply_group(
        x = sm_area(s) / sm_area(sm_ref(m), order = s) *
            sm_area(s) / sm_area(sm_seg(m), order = s), 
        groups = s[["ref_id"]], fn = max
    )
}

Dice <- function(m, ...) {
    F_measure(m, ..., alpha = 0.5)
}
