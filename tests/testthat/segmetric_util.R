#---- Util ----

.get_inter <- function(ref_sf, seg_sf) {
    inter_sf <- sf::st_intersection(ref_sf["ref_id"], seg_sf["seg_id"])
    inter_sf["inter_area"] <- sf::st_area(inter_sf)
    if (inherits(inter_sf[["inter_area"]], "units"))
        inter_sf[["inter_area"]] <- units::drop_units(inter_sf[["inter_area"]])
    inter_area <- sf::st_set_geometry(inter_sf, NULL)
    return(inter_area)
}

.get_union <- function(ref_sf, seg_sf) {
    union_sf <- sf::st_union(ref_sf["ref_id"], seg_sf["seg_id"])
    union_sf["union_area"] <- sf::st_area(union_sf)
    if (inherits(union_sf[["union_area"]], "units"))
        union_sf[["union_area"]] <- units::drop_units(union_sf[["union_area"]])
    union_area <- sf::st_set_geometry(union_sf, NULL)
    return(union_area)
}

.intersect_point_polygon <- function(x, point_sf, polygon_sf,
                                    point_id, polygon_id) {
    my_point <- point_sf[x,]
    inter <- sf::st_intersects(my_point, polygon_sf, sparse = FALSE)
    inter <- as.vector(inter)
    res <- data.frame(point_id = my_point[[point_id]],
                      polygon_ids = polygon_sf[[polygon_id]],
                      intersects = inter)
    return(res)
}

get_areas <- function(ref_sf, seg_sf) {

    suppressWarnings({

        stopifnot("Reference id not found" = "ref_id" %in% colnames(ref_sf))
        stopifnot("Segmentation id not found" = "seg_id" %in% colnames(seg_sf))
        stopifnot("Reference id must be integer" =
                      is.integer(sf::st_drop_geometry(ref_sf)[["ref_id"]]))
        stopifnot("Segmentation id must be integer" =
                      is.integer(sf::st_drop_geometry(seg_sf)[["seg_id"]]))

        ref_sf["ref_area"] = sf::st_area(ref_sf)
        if (inherits(ref_sf[["ref_area"]], "units"))
            ref_sf[["ref_area"]] <- units::drop_units(ref_sf[["ref_area"]])
        seg_sf["seg_area"] = sf::st_area(seg_sf)
        if (inherits(seg_sf[["seg_area"]], "units"))
            seg_sf[["seg_area"]] <- units::drop_units(seg_sf[["seg_area"]])
        ref_area <- sf::st_set_geometry(ref_sf, NULL)
        seg_area <- sf::st_set_geometry(seg_sf, NULL)
        inter_area <- .get_inter(ref_sf, seg_sf)
        union_area <- .get_union(ref_sf, seg_sf)

        ref_cent <- sf::st_centroid(ref_sf)
        seg_cent <- sf::st_centroid(seg_sf)
        ref_cent_inter <- lapply(seq_len(nrow(ref_cent)),
                                 FUN = .intersect_point_polygon,
                                 point_sf   = ref_cent,
                                 polygon_sf = seg_sf,
                                 point_id   = "ref_id",
                                 polygon_id = "seg_id")
        seg_cent_inter <- lapply(seq_len(nrow(seg_cent)),
                                 FUN = .intersect_point_polygon,
                                 point_sf   = seg_cent,
                                 polygon_sf = ref_sf,
                                 point_id   = "seg_id",
                                 polygon_id = "ref_id")
        ref_cent_inter <- do.call(rbind, ref_cent_inter)
        seg_cent_inter <- do.call(rbind, seg_cent_inter)

        colnames(ref_cent_inter) <- c("point_id", "polygon_id", "ref_cent_seg_pol")
        colnames(seg_cent_inter) <- c("point_id", "polygon_id", "seg_cent_ref_pol")

        inun <- merge(inter_area, union_area,
                      by = c("ref_id", "seg_id"),
                      all.x = TRUE,
                      all.y = FALSE)

        inun_ref <- merge(inun, ref_area,
                          by = "ref_id",
                          all.x = TRUE,
                          all.y = FALSE)

        area_df <- merge(inun_ref, seg_area,
                         by = "seg_id",
                         all.x = TRUE,
                         all.y = FALSE)

        area_df <- merge(area_df, ref_cent_inter,
                         by.x = c("ref_id", "seg_id"),
                         by.y = c("point_id", "polygon_id"))

        area_df <- merge(area_df, seg_cent_inter,
                         by.x = c("seg_id", "ref_id"),
                         by.y = c("point_id", "polygon_id"))

        # Compute the distance from each centroid to the closest polygon.
        dist_mt <- sf::st_distance(ref_cent, seg_cent)
        area_df["cent_dist"] <- dist_mt[as.matrix(area_df[c("ref_id",
                                                            "seg_id")])]

    })

    return(area_df)
}



#---- Universes -----

test_x_prime <- function(area_df) {
    area_df %>%
        dplyr::group_by(seg_id) %>%
        dplyr::slice_max(inter_area) %>%
        return()
}

test_y_prime <- function(area_df) {
    area_df %>%
        dplyr::group_by(ref_id) %>%
        dplyr::slice_max(inter_area) %>%
        return()
}

test_y_a <- function(area_df) {
    area_df %>%
        dplyr::filter(ref_cent_seg_pol == TRUE) %>%
        return()
}

test_y_b <- function(area_df) {
    area_df %>%
        dplyr::filter(seg_cent_ref_pol == TRUE) %>%
        return()
}

test_y_c <- function(area_df) {
    area_df %>%
        dplyr::mutate(yc = inter_area / seg_area) %>%
        dplyr::filter(yc > 0.5) %>%
        return()
}

test_y_d <- function(area_df) {
    area_df %>%
        dplyr::mutate(yd = inter_area / ref_area) %>%
        dplyr::filter(yd > 0.5) %>%
        return()
}

test_y_star <- function(area_df) {
    dplyr::bind_rows(test_y_a(area_df),
                     test_y_b(area_df),
                     test_y_c(area_df),
                     test_y_d(area_df)) %>%
        dplyr::distinct(seg_id, ref_id,
                        .keep_all = TRUE) %>%
        return()
}

test_y_tilde <- function(area_df) {
    area_df %>%
        dplyr::filter(inter_area > 0) %>%
        return()
}



#---- Metrics ----

test_AFI <- function(y_prime) {
    (y_prime$ref_area - y_prime$seg_area) / y_prime$ref_area
}

test_D_index <- function(y_star) {
    OS1 <- test_OS1(y_star)
    US1 <- test_US1(y_star)
    sqrt((OS1^2 + US1^2) / 2)
}

test_OS1 <- function(y_star) {
    1 - y_star$inter_area/y_star$ref_area
}

test_US1 <- function(y_star) {
    1 - y_star$inter_area/y_star$seg_area
}

test_OS2 <- function(y_prime) {
    1 - y_prime$inter_area/y_prime$ref_area
}

test_US2 <- function(y_prime) {
    1 - y_prime$inter_area/y_prime$seg_area
}

test_OS3 <- function(y_cd) {
    1 - (y_cd$inter_area / y_cd$ref_area)
}

test_US3 <- function(y_cd) {
    1 - (y_cd$inter_area / y_cd$seg_area)
}

test_overMerging <- function(y_star) {
    (y_star$seg_area - y_star$inter_area) / y_star$ref_area
}

test_underMerging <- function(y_star) {
    (y_star$ref_area - y_star$inter_area) / y_star$ref_area
}


test_QR <- function(y_star) {
    1 - (y_star$inter_area / y_star$union_area)
}


test_precision <- function(x_prime) {
    sum(x_prime$inter_area) / sum(x_prime$seg_area)
}

test_recall <- function(y_prime) {
    sum(y_prime$inter_area) / sum(y_prime$ref_area)
}

test_M <- function(y_prime) {
    sqrt(y_prime$inter_area^2 / (y_prime$ref_area * y_prime$seg_area))
}

test_RAsub <- function(y_tilde) {
    y_tilde$inter_area / y_tilde$ref_area
}

test_RAsuper <- function(y_tilde) {
    y_tilde$inter_area / y_tilde$seg_area
}

test_PI <- function(y_tilde) {
    y_tilde$inter_area^2 / (y_tilde$seg_area * y_tilde$ref_area)
}


test_ED3 <- function(y_cd) {
    sqrt((test_OS3(y_cd)^2 + test_US3(y_cd)^2) / 2)
}

test_F_measure <- function(precision, recall, alpha = 0.5) {
    1 / ((alpha / precision) + (1 - alpha) * (1 / recall))
}

test_E <- function(x_prime) {
    100 * (x_prime$seg_area - x_prime$inter_area) / x_prime$seg_area
}

test_IoU <- function(y_prime) {
    y_prime$inter_area / y_prime$union_area
}

test_SimSize <- function(y_star) {
    min(y_star$seg_area, y_star$ref_area) /
        max(y_star$seg_area, y_star$ref_area)
}

test_qLoc <- function(y_star) {
    y_star[["cent_dist"]]
}


# TODO: add new metrics.
#' - "`qLoc`"refers to quality of object’s location metric. Its optimal value
#' is 0 (Zhan et al., 2005).
#' - "`RPsub`" refers to Relative Position (sub) metric. Optimal value is 0
#' (Möller et al., 2007, Clinton et al., 2010).
#' - "`RPsuper`" refers to Relative Position (super) metric. Its values range
#' from 0 (optimal) to 1 (Möller et al., 2007, Clinton et al., 2010).
