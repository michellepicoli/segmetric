# data test
p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
p05 <- p00 + 5

test_for_all_metrics <- function(data, fn, ...) {
    
    lapply(list_metrics(), function(metric) {
        fn(get_metric(m = data, metric = metric, ...))
    })
}

test_that("empty intersection", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1.01))
    
    data <- metric(ref_sf, seg_sf)
    
    test_for_all_metrics(data = data,
                         fn = expect_error)
    
})

test_that("one vertex", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1))
    
    data <- metric(ref_sf, seg_sf)
    
    test_for_all_metrics(data = data,
                         fn = expect_error)
    
})

test_that("normal use", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
                                              p05 - 0.5,
                                              p05 + c(0.8, -0.9),
                                              p05 + c(-0.8, 0.9)))

    data <- metric(ref_sf, seg_sf)
    
    test_for_all_metrics(data = data,
                         fn = identity,
                         alpha = 0.5)
    
})

test_that("different CRS", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1.01))
    
    data <- metric(ref_sf, seg_sf)
    
    test_for_all_metrics(data = data,
                         fn = expect_error)
    
})

test_that("perfect fit", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1.01))
    
    data <- metric(ref_sf, seg_sf)
    
    test_for_all_metrics(data = data,
                         fn = expect_error)
    
})

test_that("two segments inside", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1.01))
    
    data <- metric(ref_sf, seg_sf)
    
    test_for_all_metrics(data = data,
                         fn = expect_error)
    
})

test_that("test grid", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1.01))
    
    data <- metric(ref_sf, seg_sf)
    
    test_for_all_metrics(data = data,
                         fn = expect_error)
    
})

# 
# #---- Utilitary ----
# 
# .get_inter <- function(ref_sf, seg_sf) {
#     inter_sf <- sf::st_intersection(ref_sf["ref_id"], seg_sf["seg_id"])
#     inter_sf["inter_area"] <- sf::st_area(inter_sf)
#     inter_area <- sf::st_set_geometry(inter_sf, NULL)
#     return(inter_area)
# }
# 
# .get_union <- function(ref_sf, seg_sf) {
#     union_sf <- sf::st_union(ref_sf["ref_id"], seg_sf["seg_id"])
#     union_sf["union_area"] <- sf::st_area(union_sf)
#     union_area <- sf::st_set_geometry(union_sf, NULL)
#     return(union_area)
# }
# 
# get_areas <- function(ref_sf, seg_sf) {
#     stopifnot("ref_id" %in% colnames(ref_sf))
#     stopifnot("seg_id" %in% colnames(seg_sf))
#     
#     ref_sf["ref_area"] = sf::st_area(ref_sf)
#     seg_sf["seg_area"] = sf::st_area(seg_sf)
#     ref_area <- sf::st_set_geometry(ref_sf, NULL)
#     seg_area <- sf::st_set_geometry(seg_sf, NULL)
#     inter_area <- .get_inter(ref_sf, seg_sf)
#     union_area <- .get_union(ref_sf, seg_sf)
#     
#     inun <- merge(inter_area, union_area, 
#                   by = c("ref_id", "seg_id"),
#                   all.x = TRUE,
#                   all.y = FALSE)
#     
#     inun_ref <- merge(inun, ref_area, 
#                         by = "ref_id",
#                         all.x = TRUE,
#                         all.y = FALSE)
#     
#     area_df <- merge(inun_ref, seg_area, 
#                      by = "seg_id",
#                      all.x = TRUE,
#                      all.y = FALSE)
#     
#     return(area_df)
# }
# 
# #--- test empty ----
# 
# 
# 
# ref_sf["ref_id"] = 1:nrow(ref_sf)
# seg_sf["seg_id"] = 1:nrow(seg_sf)
# 
# sf::st_crs(ref_sf) <- sf::st_crs("EPSG:32721")
# sf::st_crs(seg_sf) <- sf::st_crs("EPSG:32721")
# 
# area_df <- get_areas(ref_sf, seg_sf)
# 
# 
# 
# #--- test one vertex ----
# 
# ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
# seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1))
# 
# plot(seg_sf, border = "red", 
#      xlim = c(5, 7), 
#      ylim = c(5, 7))
# plot(ref_sf, border = "blue", add = TRUE)
# 
# ref_sf["ref_id"] = 1:nrow(ref_sf)
# seg_sf["seg_id"] = 1:nrow(seg_sf)
# 
# sf::st_crs(ref_sf) <- sf::st_crs("EPSG:32721")
# sf::st_crs(seg_sf) <- sf::st_crs("EPSG:32721")
# 
# area_df <- get_areas(ref_sf, seg_sf)
# 
# 
# 
# #--- test normal use ----
# 
# ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
# seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
#                                           p05 - 0.5,
#                                           p05 + c(0.8, -0.9),
#                                           p05 + c(-0.8, 0.9)))
# 
# plot(seg_sf, border = "red", 
#      xlim = c(4, 7), 
#      ylim = c(4, 7))
# plot(ref_sf, border = "blue", add = TRUE)
# 
# ref_sf["ref_id"] = 1:nrow(ref_sf)
# seg_sf["seg_id"] = 1:nrow(seg_sf)
# 
# sf::st_crs(ref_sf) <- sf::st_crs("EPSG:32721")
# sf::st_crs(seg_sf) <- sf::st_crs("EPSG:32721")
# 
# area_df <- get_areas(ref_sf, seg_sf)
# 
# 
# 
# #--- test different CRS ----
# 
# ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
# seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
#                                           p05 - 0.5,
#                                           p05 + c(0.8, -0.9),
#                                           p05 + c(-0.8, 0.9)))
# 
# ref_sf["ref_id"] = 1:nrow(ref_sf)
# seg_sf["seg_id"] = 1:nrow(seg_sf)
# 
# sf::st_crs(ref_sf) <- sf::st_crs("EPSG:32621")
# sf::st_crs(seg_sf) <- sf::st_crs("EPSG:32721")
# 
# ref_sf["area"] = sf::st_area(ref_sf)
# seg_sf["area"] = sf::st_area(seg_sf)
# 
# # NOTE: Error
# #inter_sf <- sf::st_intersection(ref_sf["ref_id"], seg_sf["seg_id"])
# 
# # NOTE: Error
# # union_sf <- sf::st_union(ref_sf["ref_id"], seg_sf["seg_id"])
# 
# # NOTE: Error
# #area_df <- get_areas(ref_sf, seg_sf)
# 
# 
# 
# #--- test perfect fit ----
# 
# ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
# seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
# 
# plot(seg_sf, border = "red")
# plot(ref_sf, border = "blue", add = TRUE)
# 
# plot(seg_sf, border = "red", 
#      xlim = c(5, 6), 
#      ylim = c(5, 6))
# plot(ref_sf, border = "blue", add = TRUE)
# 
# ref_sf["ref_id"] = 1:nrow(ref_sf)
# seg_sf["seg_id"] = 1:nrow(seg_sf)
# 
# sf::st_crs(ref_sf) <- sf::st_crs("EPSG:32721")
# sf::st_crs(seg_sf) <- sf::st_crs("EPSG:32721")
# 
# area_df <- get_areas(ref_sf, seg_sf)
# 
# 
# 
# #--- test two segs inside a ref ----
# 
# ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05 * 10))
# seg_sf <- sf::st_sf(geometry = sf::st_sfc(p00 + 51,
#                                           (p00 * 2) + 57))
# 
# plot(seg_sf, border = "red", 
#      xlim = c(50, 60), 
#      ylim = c(50, 60))
# plot(ref_sf, border = "blue", add = TRUE)
# 
# ref_sf["ref_id"] = 1:nrow(ref_sf)
# seg_sf["seg_id"] = 1:nrow(seg_sf)
# 
# sf::st_crs(ref_sf) <- sf::st_crs("EPSG:32721")
# sf::st_crs(seg_sf) <- sf::st_crs("EPSG:32721")
# 
# area_df <- get_areas(ref_sf, seg_sf)
# 
# 
# 
# #--- test grid ----
# 
# ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
# seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + c(-0.5, -0.5),
#                                           p05 + c( 0.5, -0.5),
#                                           p05 + c( 0.5,  0.5),
#                                           p05 + c(-0.5,  0.5)))
# 
# plot(seg_sf, border = "red", 
#      xlim = c(4, 7), 
#      ylim = c(4, 7))
# plot(ref_sf, border = "blue", add = TRUE)
# 
# ref_sf["ref_id"] = 1:nrow(ref_sf)
# seg_sf["seg_id"] = 1:nrow(seg_sf)
# 
# sf::st_crs(ref_sf) <- sf::st_crs("EPSG:32721")
# sf::st_crs(seg_sf) <- sf::st_crs("EPSG:32721")
# 
# area_df <- get_areas(ref_sf, seg_sf)
# 

