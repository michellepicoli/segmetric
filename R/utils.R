
#' @export
multi_metrics <- function(m, metrics, ...) {
    
    unlist(lapply(metrics, function(metric) {
        summary(compute_metric(m, metric = metric, ...))
    }))
}

