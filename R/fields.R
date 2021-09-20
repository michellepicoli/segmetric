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

    Y_tilde <- Y_tilde(m)
    res <- intersection(x = ref_centroids(m), y = seg_sf(m))
    res <- Y_tilde[rows_inset(Y_tilde, res),]
    .metric_set(m, field, value = res)
    res
}

Y_b <- function(m) {
    field <- "Y_b"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    Y_tilde <- Y_tilde(m)
    res <- intersection(x = seg_centroids(m), y = ref_sf(m))
    res <- Y_tilde[rows_inset(Y_tilde, res),]
    .metric_set(m, field, value = res)
    res
}

Y_c <- function(m) {
    field <- "Y_c"
    if (.metric_exists(m, field))
        return(.metric_get(m, field))

    Y_tilde <- Y_tilde(m)
    seg_area <- area(seg_sf, order = seg_id(Y_tilde))
    inter_area <- area(Y_tilde)

    Y_tilde[inter_area / seg_area > 0.5,]
    .metric_set(m, field, value = res)
    res
}

# 
# 
#         "Y_c" = list(
#             depends    = c("Y_tilde"),
#             expression = quote({
# 
#                 seg_area <- area(seg_sf, order = seg_id(Y_tilde))
#                 inter_area <- area(Y_tilde)
# 
#                 Y_tilde[inter_area / seg_area > 0.5,]
#             })
#         ),
#         "Y_d" = list(
#             depends    = c("Y_tilde"),
#             expression = quote({
# 
#                 ref_area <- area(ref_sf, order = ref_id(Y_tilde))
#                 inter_area <- area(Y_tilde)
# 
#                 Y_tilde[inter_area / ref_area > 0.5,]
#             })
#         ),
#         "Y_star" = list(
#             depends    = c("Y_a", "Y_b", "Y_c", "Y_d"),
#             expression = quote({
# 
#                 bind_all(Y_a, Y_b, Y_c, Y_d)
#             })
#         ),
#         "Y_cd" = list(
#             depends    = c("Y_c", "Y_d"),
#             expression = quote({
# 
#                 bind_all(Y_c, Y_d)
#             })
#         ),
#         "Y_e" = list(
#             depends    = c("Y_tilde"),
#             expression = quote({
# 
#                 seg_area <- area(seg_sf, order = seg_id(Y_tilde))
#                 inter_area <- area(Y_tilde)
# 
#                 Y_tilde[inter_area / seg_area == 1,]
#             })
#         ),
#         "Y_f" = list(
#             depends    = c("Y_tilde"),
#             expression = quote({
# 
#                 seg_area <- area(seg_sf, order = seg_id(Y_tilde))
#                 inter_area <- area(Y_tilde)
# 
#                 Y_tilde[inter_area / seg_area > 0.55,]
#             })
#         ),
#         "Y_g" = list(
#             depends    = c("Y_tilde"),
#             expression = quote({
# 
#                 seg_area <- area(seg_sf, order = seg_id(Y_tilde))
#                 inter_area <- area(Y_tilde)
# 
#                 Y_tilde[inter_area / seg_area > 0.75,]
#             })
#         )),
#     class = "db_metric"
# )
