ref_sf  <- function(m) {
    .metric_get(m, "ref_sf")
}

seg_sf <- function(m) {
    .metric_get(m, "seg_sf")
}

Y_tilde <- function(m) {
    field <- "Y_tilde"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    res <- intersection(x = ref_sf(m), y = seg_sf(m), touches = FALSE)
    .metric_set(m, field, value = res)
    res
}

X_tilde <- function(m) {
    field <- "X_tilde"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    res <- intersection(x = seg_sf(m), y = ref_sf(m), touches = FALSE)
    .metric_set(m, field, value = res)
    res
}

Y_prime <- function(m) {
    field <- "Y_prime"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    res <- Y_tilde(m) %>%
        dplyr::mutate(inter_area = area(.)) %>%
        dplyr::group_by(ref_id) %>%
        dplyr::filter(inter_area == max(inter_area)) %>%
        dplyr::select(-inter_area) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    class(res) <- c("universe_sf", class(res))
    .metric_set(m, field, value = res)
    res
}

X_prime <- function(m) {
    field <- "X_prime"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    res <- X_tilde(m) %>%
        dplyr::mutate(inter_area = area(.)) %>%
        dplyr::group_by(seg_id) %>%
        dplyr::filter(inter_area == max(inter_area)) %>%
        dplyr::select(-inter_area) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    class(res) <- c("universe_sf", class(res))
    .metric_set(m, field, value = res)
    res
}

ref_centroids <- function(m) {
    field <- "ref_centroids"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    res <- centroid(ref_sf(m))
    .metric_set(m, field, value = res)
    res
}

seg_centroids <- function(m) {
    field <- "seg_centroids"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    res <- centroid(seg_sf(m))
    .metric_set(m, field, value = res)
    res
}

Y_a <- function(m) {
    field <- "Y_a"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    Y <- Y_tilde(m)
    sel <- Y %inset% intersection(x = ref_centroids(m), y = seg_sf(m))
    res <- suppressWarnings(
        Y[sel,]
    )
    .metric_set(m, field = field, value = res)
    res
}

Y_b <- function(m) {
    field <- "Y_b"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    Y <- Y_tilde(m)
    sel <- Y %inset% intersection(x = seg_centroids(m), y = ref_sf(m))
    res <- suppressWarnings(
        Y[sel,]
    )
    .metric_set(m, field, value = res)
    res
}

Y_c <- function(m) {
    field <- "Y_c"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    Y <- Y_tilde(m)
    sel <- area(Y) / area(seg_sf(m), order = seg_id(Y)) > 0.5
    res <- suppressWarnings(
        Y[sel,]
    )
    .metric_set(m, field, value = res)
    res
}

Y_d <- function(m) {
    field <- "Y_d"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))
    
    Y <- Y_tilde(m)
    sel <- area(Y) / area(ref_sf(m), order = ref_id(Y)) > 0.5
    res <- suppressWarnings(
        Y[sel,]
    )
    .metric_set(m, field, value = res)
    res
}

Y_star <- function(m) {
    field <- "Y_star"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))
    
    res <- bind_all(Y_a(m), Y_b(m), Y_c(m), Y_d(m))
    .metric_set(m, field, value = res)
    res
}

Y_cd <- function(m) {
    field <- "Y_cd"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))
    
    res <- bind_all(Y_c(m), Y_d(m))
    .metric_set(m, field, value = res)
    res
}

Y_e <- function(m) {
    field <- "Y_e"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))
    
    Y <- Y_tilde(m)
    seg_area <- area(seg_sf(m), order = seg_id(Y))
    inter_area <- area(Y)
    res <- suppressWarnings(
        Y[inter_area / seg_area == 1,]
    )
    .metric_set(m, field, value = res)
    res
}

Y_f <- function(m) {
    field <- "Y_f"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))
    
    Y <- Y_tilde(m)
    seg_area <- area(seg_sf(m), order = seg_id(Y))
    inter_area <- area(Y)
    res <- suppressWarnings(
        Y[inter_area / seg_area == 0.55,]
    )
    .metric_set(m, field, value = res)
    res
}

Y_g <- function(m) {
    field <- "Y_f"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))
    
    Y <- Y_tilde(m)
    seg_area <- area(seg_sf(m), order = seg_id(Y))
    inter_area <- area(Y)
    res <- suppressWarnings(
        Y[inter_area / seg_area == 0.75,]
    )
    .metric_set(m, field, value = res)
    res
}
