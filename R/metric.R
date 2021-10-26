
.metric_check <- function(m, len = NULL) {
    
    stopifnot(inherits(m, "metric"))
    stopifnot(length(m) <= 1)
    stopifnot(all(c("ref_sf", "seg_sf") %in% .metric_fields(m)))
    if (length(m) == 1) {
        stopifnot(!is.null(names(m)))
        stopifnot(all(names(m) %in% list_metrics()))
    }
    if (!is.null(len))
        stopifnot(length(m) == len)
}

.metric_env <- function(m) {
    attr(m, which = ".env", exact = TRUE)
}

.metric_eval <- function(m, fn, parameters = list()) {
    do.call(fn, args = c(list(m = m), parameters))
}

.metric_fields <- function(m) {
    ls(.metric_env(m))
}

.metric_exists <- function(m, field) {
    exists(field, envir = .metric_env(m), inherits = FALSE)
}

.metric_set <- function(m, field, value) {
    assign(field, value, envir = .metric_env(m))
    m
}

.metric_get <- function(m, field) {
    get(field, envir = .metric_env(m), inherits = FALSE)
}

.metric_clear <- function(m) {
    fields <- .metric_fields(m)
    fields <- fields[!fields %in% c("ref_sf", "seg_sf")]
    rm(list = fields, envir = .metric_env(m), inherits = FALSE)
    invisible(NULL)
}

.metric_compute <- function(m, metric, parameters = list()) {
    
    f <- .db_get(key = metric)
    
    m[[1]] <- .metric_eval(m = m, fn = f[["fn"]], 
                           parameters = parameters)
    names(m) <- metric
    m
}

#' @export
metric <- function(ref_sf, seg_sf) {
    
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
              class = c("metric"))
}


#' @export
get_metric <- function(m, metric, ...) {
    
    .metric_check(m = m)
    stopifnot(metric %in% list_metrics())
    
    .metric_compute(m = m, metric = metric, parameters = list(...))
}

#' @export
multi_metrics <- function(m, metrics, ...) {
    
    unlist(lapply(metrics, function(metric) {
        summary(get_metric(m, metric = metric, ...))
    }))
}

#' @export
get_ref_area <- function(m) {
    
    .metric_check(m = m, len = 1)
    
    f <- .db_get(key = names(m))
    ordering <- f[["depends"]][[1]]
    
    ref_sf <- .metric_get(m = m, field = "ref_sf")
    ref_rows <- ref_id(.metric_get(m = m, field = ordering))
    
    area(ref_sf, order = ref_rows)
}

#' @export
get_seg_area <- function(m) {
    .metric_check(m = m, len = 1)
    
    f <- .db_get(key = names(m))
    ordering <- f[["depends"]][[1]]
    
    seg_sf <- .metric_get(m = m, field = "seg_sf")
    seg_rows <- seg_id(.metric_get(m = m, field = ordering))
    
    area(seg_sf, order = seg_rows)
}

#' @export
get_inter_area <- function(m) {
    .metric_check(m = m, len = 1)
    
    f <- .db_get(key = names(m))
    field <- f[["depends"]][[1]]
    
    area(.metric_get(m = m, field = field))
}

#' @exportS3Method
plot.metric <- function(m, ...) {
    
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
summary.metric <- function(m, w = NULL, ...) {
    
    stopifnot(inherits(m, "metric"))
    
    if (!is.null(w))
        return(lapply(m, weighted.mean, w = w))
    
    lapply(m, mean)
}

