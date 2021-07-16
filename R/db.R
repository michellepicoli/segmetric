# TODO:
# - This code computes metrics twice. Optimize! (line 41 --------). A possible solution would be to change lapply for a for loop, lines 66 & 77.
# - Separate summary from get_metric. i.e summarize_metric?
# - Do we need to compute more then one metric at once?
# - Check if summarize works for all metrics.
# - Add metrics from Costa's paper (Supervised methods of image segmentation accuracy assessment in land cover mapping).
# - Update vignette & examples.


db_universe <- list(
    "Y_tilde" = list(
        expression = quote({
            Y_tilde <- sf::st_intersection(x = ref_sf, y = seg_sf)
            list(ref_id = Y_tilde[["ref_id"]],
                 seg_id = Y_tilde[["seg_id"]])
        })
    ),
    "Y_prime" = list(
        expression = quote({
            Y_tilde <- sf::st_intersection(x = ref_sf, y = seg_sf) 
            
            Y_prime <- Y_tilde %>% 
                dplyr::mutate(inter_area = sf::st_area()) %>%
                dplyr::group_by(ref_id) %>% 
                dplyr::filter(inter_area == max(inter_area)) %>%
                dplyr::slice(1) %>% 
                dplyr::ungroup()
            
            list(ref_id = Y_prime[["ref_id"]],
                 seg_id = Y_prime[["seg_id"]])
        })
    ),
    "Y_star" = list(
        expression = quote({
            Y_tilde <- sf::st_intersection(x = ref_sf, y = seg_sf) 
            
            Y_star <- Y_tilde %>% 
                dplyr::mutate(inter_area = sf::st_area()) %>%
                dplyr::group_by(ref_id) %>% 
                dplyr::filter(inter_area == max(inter_area)) %>%
                dplyr::slice(1) %>% 
                dplyr::ungroup()
            
            list(ref_id = Y_star[["ref_id"]],
                 seg_id = Y_star[["seg_id"]])
        })
    )
)

db_metrics <- list(
    "LRE" = list(
        depends    = c("inter_area", "ref_area", "ref_id"),
        expression = quote(1 - Y_prime(inter_area/ref_area[ref_id])),
        subset_ref = NULL,
        subset_seg = "Y_prime",
        citation   = "Persello and Bruzzone (2010)"
    ),
    "oseg_per" = list(
        depends    = c("inter_area", "ref_area", "ref_id"),
        expression = quote(1 - Y_prime(inter_area/ref_area[ref_id])),
        subset_ref = NULL,
        subset_seg = "Y_prime",
        citation   = "Persello and Bruzzone (2010)"
    ),
    "oseg_cli" = list(
        depends    = c("inter_area", "ref_area", "ref_id"),
        expression = quote(1 - Y_star(inter_area/ref_area[ref_id])),
        citation   = "Clinton et al. (2010)"
    ), 
    "useg_per" = list(
        depends    = c("inter_area", "seg_area", "seg_id"),
        expression = quote(1 - Y_prime(inter_area/seg_area[seg_id])),
        citation   = "Persello and Bruzzone (2010)"
    ),
    "useg_cli" = list(
        depends    = c("inter_area", "seg_area", "seg_id"),
        expression = quote(1 - Y_star(inter_area/seg_area[seg_id])),
        citation   = "Clinton et al. (2010)"
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
