# TODO:
# - This code computes metrics twice. Optimize! (line 41 --------). A possible solution would be to change lapply for a for loop, lines 66 & 77.
# - Separate summary from get_metric. i.e summarize_metric?
# - Do we need to compute more then one metric at once?
# - Check if summarize works for all metrics.
# - Add metrics from Costa's paper (Supervised methods of image segmentation accuracy assessment in land cover mapping).
# - Update vignette & examples.

rows_inset <- function(x, y) {
    
    which(paste0(x$seg_id, ";", x$ref_id) %in% paste0(y$seg_id, ";", y$ref_id))
}

rows_distinct <- function(x) {
    
    id <- paste0(x$seg_id, ";", x$ref_id)
    match(unique(id), id)
}

area <- function(data, order = NULL) {
    area <- units::drop_units(sf::st_area(data))
    if (!is.null(order))
        return(area[order])
    return(area)
}

ref_id <- function(data) data[["ref_id"]]

seg_id <- function(data) data[["seg_id"]]

db_metrics <- list(
    "oseg_per" = list(
        depends    = c("Y_prime"),
        expression = quote({
            1 - area(Y_prime) / area(ref_sf, order = ref_id(Y_prime))
        }),
        citation   = "Persello and Bruzzone (2010)"
    ),
    "oseg_cli" = list(
        depends    = c("Y_star"),
        expression = quote({
            1 - area(Y_star) / area(ref_sf, order = ref_id(Y_star))
        }),
        citation   = "Clinton et al. (2010)"
    ), 
    "useg_per" = list(
        depends    = c("Y_prime"),
        expression = quote({
            1 - area(Y_prime) / area(seg_sf, order = seg_id(Y_prime))
        }),
        citation   = "Persello and Bruzzone (2010)"
    ),
    "useg_cli" = list(
        depends    = c("Y_star"),
        expression = quote({
            1 - area(Y_star) / area(seg_sf, order = seg_id(Y_star))
        }),
        citation   = "Clinton et al. (2010)"
    )
)

db_fields <- list(
    "Y_tilde" = list(
        depends    = character(),
        expression = quote({
            
            sf::st_intersection(x = ref_sf, y = seg_sf)
        })
    ),
    "Y_prime" = list(
        depends    = c("Y_tilde"),
        expression = quote({
            
            Y_tilde %>% 
                dplyr::mutate(inter_area = sf::st_area(.)) %>%
                dplyr::group_by(ref_id) %>% 
                dplyr::filter(inter_area == max(inter_area)) %>%
                dplyr::slice(1) %>% 
                dplyr::ungroup()
        })
    ),
    "ref_centroids" = list(
        depends    = character(),
        expression = quote({
            
            sf::st_centroid(ref_sf)
        })
    ),
    "Y_a" = list(
        depends    = c("Y_tilde", "ref_centroids"),
        expression = quote({
            
            Y_a <- sf::st_intersection(x = ref_centroids,
                                       y = seg_sf)
            
            Y_tilde[rows_inset(Y_tilde, Y_a),]
        })
    ),
    "seg_centroids" = list(
        depends    = character(),
        expression = quote({
            
            sf::st_centroid(seg_sf)
        })
    ),
    "Y_b" = list(
        depends    = c("Y_tilde", "seg_centroids"),
        expression = quote({
            
            Y_b <- sf::st_intersection(x = seg_centroids,
                                       y = ref_sf)
            
            Y_tilde[rows_inset(Y_tilde, Y_b),]
        })
    ),
    "Y_c" = list(
        depends    = c("Y_tilde"),
        expression = quote({
            
            seg_area <- area(seg_sf, order = seg_id(Y_tilde))
            inter_area <- area(Y_tilde)
            
            Y_tilde[inter_area / seg_area > 0.5]
        })
    ),
    "Y_d" = list(
        depends    = c("Y_tilde"),
        expression = quote({
            
            ref_area <- area(ref_sf, order = ref_id(Y_tilde))
            inter_area <- area(Y_tilde)
            
            Y_tilde[inter_area / ref_area > 0.5]
        })
    ),
    "Y_star" = list(
        depends    = c("Y_a", "Y_b", "Y_c", "Y_d"),
        expression = quote({
            
            Y_star <- do.call(rbind, args = list(Y_a, Y_b, Y_c, Y_d))
            Y_star[rows_distinct(Y_star),]
        })
    ),
    "Y_e" = list(
        depends    = c("Y_tilde"),
        expression = quote({
            
            seg_area <- area(seg_sf, order = seg_id(Y_tilde))
            inter_area <- area(Y_tilde)
            
            Y_tilde[inter_area / seg_area == 1,]
        })
    ),
    "Y_f" = list(
        depends    = c("Y_tilde"),
        expression = quote({
            
            seg_area <- area(seg_sf, order = seg_id(Y_tilde))
            inter_area <- area(Y_tilde)
            
            Y_tilde[inter_area / seg_area > 0.55,]
        })
    ),
    "Y_g" = list(
        depends    = c("Y_tilde"),
        expression = quote({
            
            seg_area <- area(seg_sf, order = seg_id(Y_tilde))
            inter_area <- area(Y_tilde)
            
            Y_tilde[inter_area / seg_area > 0.75,]
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
    
    stopifnot(field %in% names(db_fields))
    f <- db_fields[[field]]
    env <- data
    for (field in f[["depends"]]) {
        if (field %in% names(env)) next
        env <- c(env, .compute_field(field, env))
    }
    env[[field]] <- eval(f[["expression"]], envir = env)
    return(env)
}

.compute_metric <- function(metric, data) {
    
    stopifnot(field %in% names(db_metrics))
    m <- db_metrics[[metric]]
    env <- data
    for (field in m[["depends"]]) {
        if (field %in% names(env)) next
        env <- c(env, .compute_field(field, env))
    }
    env[[metric]] <- eval(m[["expression"]], envir = env)
    
    return(res)
}

metric <- function(ref_sf, seg_sf) {
    
    if (is.character(ref_sf))
        ref_sf <- sf::read_sf(ref_sf)
    stopifnot(inherits(ref_sf, "sf"))

    if (is.character(seg_sf))
        seg_sf <- sf::read_sf(seg_sf)
    stopifnot(inherits(seg_sf, "sf"))
    
    ref_sf[["ref_id"]] <- seq_len(nrow(ref_sf))
    seg_sf[["seg_id"]] <- seq_len(nrow(seg_sf))
    data <- list(ref_sf = ref_sf, seg_sf = seg_sf)
    class(data) <- c("segmetric", class(data))
    data
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
