
#' @export
multi_metrics <- function(m, metrics, ...) {
    
    unlist(lapply(metrics, function(metric) {
        summary(get_metric(m, metric = metric, ...))
    }))
}

