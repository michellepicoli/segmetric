# TODO:
# - This code computes metrics twice. Optimize! (line 41 --------). A possible solution would be to change lapply for a for loop, lines 66 & 77.
# - Separate summary from get_metric. i.e summarize_metric?
# - Do we need to compute more then one metric at once?
# - Check if summarize works for all metrics.
# - Add metrics from Costa's paper (Supervised methods of image segmentation accuracy assessment in land cover mapping).
# - Update vignette & examples.

db_metrics <- list(
    "oseg" = list(
        depends    = c("inter_area", "ref_area", "ref_id"),
        expression = quote(inter_area/ref_area[ref_id])
    ), 
    "useg" = list(
        depends    = c("inter_area", "seg_area", "seg_id"),
        expression = quote(inter_area/seg_area[seg_id])
    )
)

db_fields <- list(
    "seg_id" = list(
        depends = c("inter_sf"), 
        expression = quote(inter_sf[["seg_id"]])
    ),
    "ref_id" = list(
        depends = c("inter_sf"), 
        expression = quote(inter_sf[["ref_id"]])
    ),
    "inter_area" = list(
        depends    = c("inter_sf"),
        expression = quote(sf::st_area(inter_sf))
    ), 
    "ref_area"   = list(
        depends = c(),
        expression = quote(sf::st_area(ref_sf))
    ), 
    "seg_area"   = list(
        depends = c(),
        expression = quote(sf::st_area(seg_sf))
    ),
    "inter_sf" = list(
        depends = c(),
        expression = quote({
            print("----------------")
            sf::st_intersection(x = ref_sf, y = seg_sf)
            })
    )
)

db_summary = list(
    "mean"   = list(
        expression = quote(mean(metric, na.rm = TRUE))
    ),
    "w_mean" = list(
        expression = quote(weighted.mean(metric, weight, na.rm = TRUE))
    ),
    "identity" = list(
        expression = quote(metric)
    )
)

.compute_field <- function(field, data){
    if (field %in% names(data))
        return(NULL)
    stopifnot(field %in% names(db_fields))
    f <- db_fields[[field]]
    res <- unlist(lapply(f[["depends"]], .compute_field, data), 
                  recursive = FALSE)
    names(res) <- f[["depends"]]
    res[[field]] <- eval(f[["expression"]], envir = c(data, res))
    return(res)
}

.compute_metric <- function(metric, data) {
    data[["ref_sf"]][["ref_id"]] <- seq_len(nrow(data[["ref_sf"]]))
    data[["seg_sf"]][["seg_id"]] <- seq_len(nrow(data[["seg_sf"]]))
    m <- db_metrics[[metric]] 
    res <- unlist(lapply(m[["depends"]], .compute_field, data),
                  recursive = FALSE)
    res[[metric]] <- eval(m[["expression"]], envir = res)
    return(res)
}

#' @export
get_metric <- function(data, metric, summary, weight_field = NULL) {
    if (summary == "w_mean")
        stopifnot(!is.null(weight_field))
    stopifnot(metric %in% names(db_metrics))
    stopifnot(summary %in% names(db_summary))
    s <- db_summary[[summary]]
    res <- .compute_metric(metric, data)
    res[["metric"]] <- res[[metric]]
    if (summary == "w_mean") {
        stopifnot(weight_field %in% names(res))
        res[["weight"]] <- res[[weight_field]]
    }
    eval(s[["expression"]], envir = res)
}
