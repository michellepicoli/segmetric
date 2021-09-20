# TODO: make a plot function plot.metric (S3method)

# data test
p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
p05 <- p00 + 5

test_that("empty intersection", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1.01))
    
    data <- metric(ref_sf, seg_sf)
    
    get_metric(data, "OS2")
    get_metric(data, "OS1") 
    get_metric(data, "US2") 
    get_metric(data, "US1") 
    get_metric(data, "AFI") 
    get_metric(data, "QR") 
    get_metric(data, "D_index") 
    get_metric(data, "precision") 
    get_metric(data, "recall") 
    get_metric(data, "underMerging") 
    get_metric(data, "overMerging") 
    get_metric(data, "M") 
    get_metric(data, "E") 
    get_metric(data, "RAsub") 
    get_metric(data, "RAsuper") 
    get_metric(data, "PI") 
    get_metric(data, "F") 
    get_metric(data, "OS3") 
    get_metric(data, "US3") 
    get_metric(data, "ED3") 
    get_metric(data, "F_measure")
    
})

test_that("one vertex", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1))
    
    data <- metric(ref_sf, seg_sf)
    
    get_metric(data, "OS2")
    get_metric(data, "OS1") 
    get_metric(data, "US2") 
    get_metric(data, "US1") 
    get_metric(data, "AFI") 
    get_metric(data, "QR") 
    get_metric(data, "D_index") 
    get_metric(data, "precision") 
    get_metric(data, "recall") 
    get_metric(data, "underMerging") 
    get_metric(data, "overMerging") 
    get_metric(data, "M") 
    get_metric(data, "E") 
    get_metric(data, "RAsub") 
    get_metric(data, "RAsuper") 
    get_metric(data, "PI") 
    get_metric(data, "F") 
    get_metric(data, "OS3") 
    get_metric(data, "US3") 
    get_metric(data, "ED3") 
    get_metric(data, "F_measure")
    
    
})

test_that("one edge", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + c(0, 1)))
    
    data <- metric(ref_sf, seg_sf)
    
    get_metric(data, "OS2")
    get_metric(data, "OS1") 
    get_metric(data, "US2") 
    get_metric(data, "US1") 
    get_metric(data, "AFI") 
    get_metric(data, "QR") 
    get_metric(data, "D_index") 
    get_metric(data, "precision") 
    get_metric(data, "recall") 
    get_metric(data, "underMerging") 
    get_metric(data, "overMerging") 
    get_metric(data, "M") 
    get_metric(data, "E") 
    get_metric(data, "RAsub") 
    get_metric(data, "RAsuper") 
    get_metric(data, "PI") 
    get_metric(data, "F") 
    get_metric(data, "OS3") 
    get_metric(data, "US3") 
    get_metric(data, "ED3") 
    get_metric(data, "F_measure")
    
    
})

test_that("one vertex and a polygon", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1,
                                              p05 - 0.5))

    data <- metric(ref_sf, seg_sf)
    
    get_metric(data, "OS2")
    get_metric(data, "OS1") 
    get_metric(data, "US2") 
    get_metric(data, "US1") 
    get_metric(data, "AFI") 
    get_metric(data, "QR") 
    get_metric(data, "D_index") 
    get_metric(data, "precision") 
    get_metric(data, "recall") 
    get_metric(data, "underMerging") 
    get_metric(data, "overMerging") 
    get_metric(data, "M") 
    get_metric(data, "E") 
    get_metric(data, "RAsub") 
    get_metric(data, "RAsuper") 
    get_metric(data, "PI") 
    get_metric(data, "F") 
    get_metric(data, "OS3") 
    get_metric(data, "US3") 
    get_metric(data, "ED3") 
    get_metric(data, "F_measure")
    
    
})

test_that("normal use", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05 * 1))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
                                              p05 - 0.5,
                                              p05 + c(0.8, -0.9),
                                              p05 + c(-0.8, 0.9)))

    
    data <- metric(ref_sf, seg_sf)
    
    get_areas(ref_sf(data), seg_sf(data))
    
    
    get_metric(data, "OS2")
    get_metric(data, "OS1") 
    get_metric(data, "US2") 
    get_metric(data, "US1") 
    get_metric(data, "AFI") 
    get_metric(data, "QR") 
    get_metric(data, "D_index") 
    get_metric(data, "precision") 
    get_metric(data, "recall") 
    get_metric(data, "underMerging") 
    get_metric(data, "overMerging") 
    get_metric(data, "M") 
    get_metric(data, "E") 
    get_metric(data, "RAsub") 
    get_metric(data, "RAsuper") 
    get_metric(data, "PI") 
    get_metric(data, "F") 
    get_metric(data, "OS3") 
    get_metric(data, "US3") 
    get_metric(data, "ED3") 
    get_metric(data, "F_measure")
    
})

test_that("different CRS", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
                                              p05 - 0.5,
                                              p05 + c(0.8, -0.9),
                                              p05 + c(-0.8, 0.9)))
    
    data <- metric(ref_sf, seg_sf)
    
    get_metric(data, "OS2")
    get_metric(data, "OS1") 
    get_metric(data, "US2") 
    get_metric(data, "US1") 
    get_metric(data, "AFI") 
    get_metric(data, "QR") 
    get_metric(data, "D_index") 
    get_metric(data, "precision") 
    get_metric(data, "recall") 
    get_metric(data, "underMerging") 
    get_metric(data, "overMerging") 
    get_metric(data, "M") 
    get_metric(data, "E") 
    get_metric(data, "RAsub") 
    get_metric(data, "RAsuper") 
    get_metric(data, "PI") 
    get_metric(data, "F") 
    get_metric(data, "OS3") 
    get_metric(data, "US3") 
    get_metric(data, "ED3") 
    get_metric(data, "F_measure")
    
    
})

test_that("perfect fit", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    
    data <- metric(ref_sf, seg_sf)
    
    get_metric(data, "OS2")
    get_metric(data, "OS1") 
    get_metric(data, "US2") 
    get_metric(data, "US1") 
    get_metric(data, "AFI") 
    get_metric(data, "QR") 
    get_metric(data, "D_index") 
    get_metric(data, "precision") 
    get_metric(data, "recall") 
    get_metric(data, "underMerging") 
    get_metric(data, "overMerging") 
    get_metric(data, "M") 
    get_metric(data, "E") 
    get_metric(data, "RAsub") 
    get_metric(data, "RAsuper") 
    get_metric(data, "PI") 
    get_metric(data, "F") 
    get_metric(data, "OS3") 
    get_metric(data, "US3") 
    get_metric(data, "ED3") 
    get_metric(data, "F_measure")
    
    
})

test_that("two segments inside", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05 * 10))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p00 + 51,
                                              (p00 * 2) + 57))
    
    data <- metric(ref_sf, seg_sf)
    
    get_metric(data, "OS2")
    get_metric(data, "OS1") 
    get_metric(data, "US2") 
    get_metric(data, "US1") 
    get_metric(data, "AFI") 
    get_metric(data, "QR") 
    get_metric(data, "D_index") 
    get_metric(data, "precision") 
    get_metric(data, "recall") 
    get_metric(data, "underMerging") 
    get_metric(data, "overMerging") 
    get_metric(data, "M") 
    get_metric(data, "E") 
    get_metric(data, "RAsub") 
    get_metric(data, "RAsuper") 
    get_metric(data, "PI") 
    get_metric(data, "F") 
    get_metric(data, "OS3") 
    get_metric(data, "US3") 
    get_metric(data, "ED3") 
    get_metric(data, "F_measure")
    
    
})

test_that("test grid", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + c(-0.5, -0.5),
                                              p05 + c( 0.5, -0.5),
                                              p05 + c( 0.5,  0.5),
                                              p05 + c(-0.5,  0.5)))
    
    data <- metric(ref_sf, seg_sf)
    
    get_metric(data, "OS2")
    get_metric(data, "OS1") 
    get_metric(data, "US2") 
    get_metric(data, "US1") 
    get_metric(data, "AFI") 
    get_metric(data, "QR") 
    get_metric(data, "D_index") 
    get_metric(data, "precision") 
    get_metric(data, "recall") 
    get_metric(data, "underMerging") 
    get_metric(data, "overMerging") 
    get_metric(data, "M") 
    get_metric(data, "E") 
    get_metric(data, "RAsub") 
    get_metric(data, "RAsuper") 
    get_metric(data, "PI") 
    get_metric(data, "F") 
    get_metric(data, "OS3") 
    get_metric(data, "US3") 
    get_metric(data, "ED3") 
    get_metric(data, "F_measure")
    
    
})

#---- Utilitary ----

.get_inter <- function(ref_sf, seg_sf) {
    inter_sf <- sf::st_intersection(ref_sf["ref_id"], seg_sf["seg_id"])
    inter_sf["inter_area"] <- sf::st_area(inter_sf)
    inter_area <- sf::st_set_geometry(inter_sf, NULL)
    return(inter_area)
}

.get_union <- function(ref_sf, seg_sf) {
    union_sf <- sf::st_union(ref_sf["ref_id"], seg_sf["seg_id"])
    union_sf["union_area"] <- sf::st_area(union_sf)
    union_area <- sf::st_set_geometry(union_sf, NULL)
    return(union_area)
}

get_areas <- function(ref_sf, seg_sf) {
    stopifnot("ref_id" %in% colnames(ref_sf))
    stopifnot("seg_id" %in% colnames(seg_sf))

    ref_sf["ref_area"] = sf::st_area(ref_sf)
    seg_sf["seg_area"] = sf::st_area(seg_sf)
    ref_area <- sf::st_set_geometry(ref_sf, NULL)
    seg_area <- sf::st_set_geometry(seg_sf, NULL)
    inter_area <- .get_inter(ref_sf, seg_sf)
    union_area <- .get_union(ref_sf, seg_sf)

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

    return(area_df)
}


