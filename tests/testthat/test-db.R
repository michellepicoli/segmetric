
source("segmetric_util.R")

test_that("different CRS test", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
                                              p05 - 0.5,
                                              p05 + c(0.8, -0.9),
                                              p05 + c(-0.8, 0.9)))
    
    sf::st_crs(ref_sf) <- 4326
    expect_error(sm_read(ref_sf, seg_sf))
    
    sf::st_crs(seg_sf) <- 6322
    expect_error(sm_read(ref_sf, seg_sf))
    
    suppressWarnings(
        sf::st_crs(seg_sf) <- 4326
    )
    expect_type(sm_read(ref_sf, seg_sf), "list")
    
})

test_that("empty intersection tests", {
    
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1.01))
    data <- sm_read(ref_sf, seg_sf)
    
    expect_true(sm_is_empty(sm_compute(data, "OS2")))
    expect_true(sm_is_empty(sm_compute(data, "OS1")))
    expect_true(sm_is_empty(sm_compute(data, "US2")))
    expect_true(sm_is_empty(sm_compute(data, "US1")))
    expect_true(sm_is_empty(sm_compute(data, "AFI")))
    expect_true(sm_is_empty(sm_compute(data, "QR")))
    expect_true(sm_is_empty(sm_compute(data, "D_index")))
    expect_true(sm_is_empty(sm_compute(data, "precision")))
    expect_true(sm_is_empty(sm_compute(data, "recall")))
    expect_true(sm_is_empty(sm_compute(data, "M")))
    expect_true(sm_is_empty(sm_compute(data, "E")))
    expect_true(sm_is_empty(sm_compute(data, "RAsub")))
    expect_true(sm_is_empty(sm_compute(data, "RAsuper")))
    expect_true(sm_is_empty(sm_compute(data, "PI")))
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_true(sm_is_empty(sm_compute(data, "F_measure")))
    expect_true(sm_is_empty(sm_compute(data, "UMerging")))
    expect_true(sm_is_empty(sm_compute(data, "OMerging")))
    expect_true(sm_is_empty(sm_compute(data, "Fitness")))
})

test_that("one vertex intersection tests", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1))
    data <- sm_read(ref_sf, seg_sf)
    
    expect_true(sm_is_empty(sm_compute(data, "OS2")))
    expect_true(sm_is_empty(sm_compute(data, "OS1")))
    expect_true(sm_is_empty(sm_compute(data, "US2")))
    expect_true(sm_is_empty(sm_compute(data, "US1")))
    expect_true(sm_is_empty(sm_compute(data, "AFI")))
    expect_true(sm_is_empty(sm_compute(data, "QR")))
    expect_true(sm_is_empty(sm_compute(data, "D_index")))
    expect_true(sm_is_empty(sm_compute(data, "precision")))
    expect_true(sm_is_empty(sm_compute(data, "recall")))
    expect_true(sm_is_empty(sm_compute(data, "M")))
    expect_true(sm_is_empty(sm_compute(data, "E")))
    expect_true(sm_is_empty(sm_compute(data, "RAsub")))
    expect_true(sm_is_empty(sm_compute(data, "RAsuper")))
    expect_true(sm_is_empty(sm_compute(data, "PI")))
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_true(sm_is_empty(sm_compute(data, "F_measure")))
    expect_true(sm_is_empty(sm_compute(data, "UMerging")))
    expect_true(sm_is_empty(sm_compute(data, "OMerging")))
    expect_true(sm_is_empty(sm_compute(data, "Fitness")))
})

test_that("one edge intersection tests", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + c(0, 1)))
    data <- sm_read(ref_sf, seg_sf)
    
    expect_true(sm_is_empty(sm_compute(data, "OS2")))
    expect_true(sm_is_empty(sm_compute(data, "OS1")))
    expect_true(sm_is_empty(sm_compute(data, "US2")))
    expect_true(sm_is_empty(sm_compute(data, "US1")))
    expect_true(sm_is_empty(sm_compute(data, "AFI")))
    expect_true(sm_is_empty(sm_compute(data, "QR")))
    expect_true(sm_is_empty(sm_compute(data, "D_index")))
    expect_true(sm_is_empty(sm_compute(data, "precision")))
    expect_true(sm_is_empty(sm_compute(data, "recall")))
    expect_true(sm_is_empty(sm_compute(data, "M")))
    expect_true(sm_is_empty(sm_compute(data, "E")))
    expect_true(sm_is_empty(sm_compute(data, "RAsub")))
    expect_true(sm_is_empty(sm_compute(data, "RAsub")))
    expect_true(sm_is_empty(sm_compute(data, "RAsuper")))
    expect_true(sm_is_empty(sm_compute(data, "PI")))
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_true(sm_is_empty(sm_compute(data, "F_measure")))
    expect_true(sm_is_empty(sm_compute(data, "UMerging")))
    expect_true(sm_is_empty(sm_compute(data, "OMerging")))
    expect_true(sm_is_empty(sm_compute(data, "Fitness")))
})

test_that("one vertex and one polygon tests", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1,
                                              p05 - 0.5))
    data <- sm_read(ref_sf, seg_sf)
    
    expect_true(!sm_is_empty(sm_compute(data, "OS2")))
    expect_true(!sm_is_empty(sm_compute(data, "OS1")))
    expect_true(!sm_is_empty(sm_compute(data, "US2")))
    expect_true(!sm_is_empty(sm_compute(data, "US1")))
    expect_true(!sm_is_empty(sm_compute(data, "AFI")))
    expect_true(!sm_is_empty(sm_compute(data, "QR")))
    expect_true(!sm_is_empty(sm_compute(data, "D_index")))
    expect_true(!sm_is_empty(sm_compute(data, "precision")))
    expect_true(!sm_is_empty(sm_compute(data, "recall")))
    expect_true(!sm_is_empty(sm_compute(data, "M")))
    expect_true(!sm_is_empty(sm_compute(data, "E")))
    expect_true(!sm_is_empty(sm_compute(data, "RAsub")))
    expect_true(!sm_is_empty(sm_compute(data, "RAsuper")))
    expect_true(!sm_is_empty(sm_compute(data, "PI")))
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_true(!sm_is_empty(sm_compute(data, "F_measure")))
    expect_true(!sm_is_empty(sm_compute(data, "UMerging")))
    expect_true(!sm_is_empty(sm_compute(data, "OMerging")))
    expect_true(!sm_is_empty(sm_compute(data, "Fitness")))
})

test_that("normal use tests", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05 * 1))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
                                              p05 - 0.5,
                                              p05 + c(0.8, -0.9),
                                              p05 + c(-0.8, 0.9)))
    data <- sm_read(ref_sf, seg_sf)
    
    expect_true(!sm_is_empty(sm_compute(data, "OS2")))
    expect_true(!sm_is_empty(sm_compute(data, "OS1")))
    expect_true(!sm_is_empty(sm_compute(data, "US2")))
    expect_true(!sm_is_empty(sm_compute(data, "US1")))
    expect_true(!sm_is_empty(sm_compute(data, "AFI")))
    expect_true(!sm_is_empty(sm_compute(data, "QR")))
    expect_true(!sm_is_empty(sm_compute(data, "D_index")))
    expect_true(!sm_is_empty(sm_compute(data, "precision")))
    expect_true(!sm_is_empty(sm_compute(data, "precision")))
    expect_true(!sm_is_empty(sm_compute(data, "recall")))
    expect_true(!sm_is_empty(sm_compute(data, "recall")))
    expect_true(!sm_is_empty(sm_compute(data, "M")))
    expect_true(!sm_is_empty(sm_compute(data, "E")))
    expect_true(!sm_is_empty(sm_compute(data, "RAsub")))
    expect_true(!sm_is_empty(sm_compute(data, "RAsuper")))
    expect_true(!sm_is_empty(sm_compute(data, "PI")))
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_true(!sm_is_empty(sm_compute(data, "F_measure")))
    expect_true(!sm_is_empty(sm_compute(data, "UMerging")))
    expect_true(!sm_is_empty(sm_compute(data, "OMerging")))
    expect_true(!sm_is_empty(sm_compute(data, "Fitness")))
})


test_that("perfect fit test", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    
    data <- sm_read(ref_sf, seg_sf)
    
    expect_true(!sm_is_empty(sm_compute(data, "OS2")))
    expect_true(!sm_is_empty(sm_compute(data, "OS1")))
    expect_true(!sm_is_empty(sm_compute(data, "US2")))
    expect_true(!sm_is_empty(sm_compute(data, "US1")))
    expect_true(!sm_is_empty(sm_compute(data, "AFI")))
    expect_true(!sm_is_empty(sm_compute(data, "QR")))
    expect_true(!sm_is_empty(sm_compute(data, "D_index")))
    expect_true(!sm_is_empty(sm_compute(data, "precision")))
    expect_true(!sm_is_empty(sm_compute(data, "recall")))
    expect_true(!sm_is_empty(sm_compute(data, "M")))
    expect_true(!sm_is_empty(sm_compute(data, "E")))
    expect_true(!sm_is_empty(sm_compute(data, "RAsub")))
    expect_true(!sm_is_empty(sm_compute(data, "RAsuper")))
    expect_true(!sm_is_empty(sm_compute(data, "PI")))
    expect_true(!sm_is_empty(sm_compute(data, "OS3")))
    expect_true(!sm_is_empty(sm_compute(data, "US3")))
    expect_true(!sm_is_empty(sm_compute(data, "ED3")))
    expect_true(!sm_is_empty(sm_compute(data, "F_measure")))
    expect_true(!sm_is_empty(sm_compute(data, "UMerging")))
    expect_true(!sm_is_empty(sm_compute(data, "OMerging")))
    expect_true(!sm_is_empty(sm_compute(data, "Fitness")))
})


test_that("two segments inside test", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05 * 10))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p00 + 51,
                                              (p00 * 2) + 57))
    
    data <- sm_read(ref_sf, seg_sf)
    
    expect_true(!sm_is_empty(sm_compute(data, "OS2")))
    expect_true(!sm_is_empty(sm_compute(data, "OS1")))
    expect_true(!sm_is_empty(sm_compute(data, "US2")))
    expect_true(!sm_is_empty(sm_compute(data, "US1")))
    expect_true(!sm_is_empty(sm_compute(data, "AFI")))
    expect_true(!sm_is_empty(sm_compute(data, "QR")))
    expect_true(!sm_is_empty(sm_compute(data, "D_index")))
    expect_true(!sm_is_empty(sm_compute(data, "precision")))
    expect_true(!sm_is_empty(sm_compute(data, "recall")))
    expect_true(!sm_is_empty(sm_compute(data, "M")))
    expect_true(!sm_is_empty(sm_compute(data, "E")))
    expect_true(!sm_is_empty(sm_compute(data, "RAsub")))
    expect_true(!sm_is_empty(sm_compute(data, "RAsuper")))
    expect_true(!sm_is_empty(sm_compute(data, "PI")))
    expect_true(!sm_is_empty(sm_compute(data, "OS3")))
    expect_true(!sm_is_empty(sm_compute(data, "US3")))
    expect_true(!sm_is_empty(sm_compute(data, "ED3")))
    expect_true(!sm_is_empty(sm_compute(data, "F_measure")))
    
})


test_that("grid test", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg <- sf::st_sf(geometry = sf::st_sfc(p05 + c(-0.5, -0.5),
                                           p05 + c( 0.5, -0.5),
                                           p05 + c( 0.5,  0.5),
                                           p05 + c(-0.5,  0.5)))
    data <- sm_read(ref, seg)
    
    expect_true(!sm_is_empty(sm_compute(data, "OS2")))
    expect_true(!sm_is_empty(sm_compute(data, "OS1")))
    expect_true(!sm_is_empty(sm_compute(data, "US2")))
    expect_true(!sm_is_empty(sm_compute(data, "US1")))
    expect_true(!sm_is_empty(sm_compute(data, "AFI")))
    expect_true(!sm_is_empty(sm_compute(data, "QR")))
    expect_true(!sm_is_empty(sm_compute(data, "D_index")))
    expect_true(!sm_is_empty(sm_compute(data, "precision")))
    expect_true(!sm_is_empty(sm_compute(data, "recall")))
    expect_true(!sm_is_empty(sm_compute(data, "M")))
    expect_true(!sm_is_empty(sm_compute(data, "E")))
    expect_true(!sm_is_empty(sm_compute(data, "RAsub")))
    expect_true(!sm_is_empty(sm_compute(data, "RAsuper")))
    expect_true(!sm_is_empty(sm_compute(data, "PI")))
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_true(!sm_is_empty(sm_compute(data, "F_measure")))
})

test_that("normal use test values", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05 * 1))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
                                              p05 - 0.5,
                                              p05 + c(0.8, -0.9),
                                              p05 + c(-0.8, 0.9)))
    data <- sm_read(ref_sf, seg_sf)
    
    area_df <- get_areas(sm_ref(data), sm_seg(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- dplyr::bind_rows(dplyr::as_tibble(test_y_c(area_df)),
                             dplyr::as_tibble(test_y_d(area_df))) %>%
        dplyr::distinct(seg_id, ref_id, .keep_all = TRUE)
    
    expect_equal(mean(test_OS2(y_prime)), summary(sm_compute(data, "OS2")))
    expect_equal(mean(test_US2(y_prime)), summary(sm_compute(data, "US2")))
    expect_equal(mean(test_OS1(y_star)), summary(sm_compute(data, "OS1")))
    expect_equal(mean(test_US1(y_star)), summary(sm_compute(data, "US1")))
    expect_equal(mean(test_overMerging(y_star)), 
                 summary(sm_compute(data, "OMerging")))
    expect_equal(mean(test_underMerging(y_star)), 
                 summary(sm_compute(data, "UMerging")))
    expect_equal(mean(test_AFI(y_prime)), summary(sm_compute(data, "AFI")))
    expect_equal(mean(test_QR(y_star)), summary(sm_compute(data, "QR")))
    expect_equal(mean(test_D_index(y_star)), 
                 summary(sm_compute(data, "D_index")))
    expect_equal(mean(test_precision(x_prime)), 
                 summary(sm_compute(data, "precision")))
    expect_equal(mean(test_recall(y_prime)), 
                 summary(sm_compute(data, "recall")))
    expect_equal(mean(test_M(y_prime)), summary(sm_compute(data, "M")))
    expect_equal(mean(test_RAsub(y_tilde)), 
                 summary(sm_compute(data, "RAsub")))
    expect_equal(mean(test_RAsuper(y_tilde)), 
                 summary(sm_compute(data, "RAsuper")))
    expect_equal(mean(test_PI(y_tilde)), summary(sm_compute(data, "PI")))
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_equal(
        mean(test_F_measure(test_precision(x_prime), test_recall(y_prime))),
        summary(sm_compute(data, "F_measure"))
    )
    expect_equal(mean(test_E(x_prime)), summary(sm_compute(data, "E")))
})

test_that("perfect fit test values", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    
    data <- sm_read(ref_sf, seg_sf)
    
    area_df <- get_areas(sm_ref(data), sm_seg(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- dplyr::bind_rows(dplyr::as_tibble(test_y_c(area_df)),
                             dplyr::as_tibble(test_y_d(area_df))) %>%
        dplyr::distinct(seg_id, ref_id, .keep_all = TRUE)
    
    expect_equal(mean(test_OS2(y_prime)), summary(sm_compute(data, "OS2")))
    expect_equal(mean(test_US2(y_prime)), summary(sm_compute(data, "US2")))
    expect_equal(mean(test_OS1(y_star) ), summary(sm_compute(data, "OS1")))
    expect_equal(mean(test_US1(y_star) ), summary(sm_compute(data, "US1")))
    expect_equal(mean(test_overMerging(y_star) ), 
                 summary(sm_compute(data, "OMerging")))
    expect_equal(mean(test_underMerging(y_star)), 
                 summary(sm_compute(data, "UMerging")))
    expect_equal(mean(test_AFI(y_prime)), summary(sm_compute(data, "AFI")))
    expect_equal(mean(test_QR(y_star)), summary(sm_compute(data, "QR")))
    expect_equal(mean(test_D_index(y_star)), 
                 summary(sm_compute(data, "D_index")))
    expect_equal(mean(test_precision(x_prime)), 
                 summary(sm_compute(data, "precision")))
    expect_equal(mean(test_recall(y_prime)), 
                 summary(sm_compute(data, "recall")))
    expect_equal(mean(test_M(y_prime)), summary(sm_compute(data, "M")))
    expect_equal(mean(test_RAsub(y_tilde)), 
                 summary(sm_compute(data, "RAsub")))
    expect_equal(mean(test_RAsuper(y_tilde)), 
                 summary(sm_compute(data, "RAsuper")))
    expect_equal(mean(test_PI(y_tilde)), summary(sm_compute(data, "PI")))
    expect_equal(mean(test_OS3(y_cd)), summary(sm_compute(data, "OS3")))
    expect_equal(mean(test_US3(y_cd)), summary(sm_compute(data, "US3")))
    expect_equal(mean(test_ED3(area_df)), summary(sm_compute(data, "ED3")))
    expect_equal(
        mean(test_F_measure(test_precision(x_prime),
                            test_recall(y_prime))), 
        summary(sm_compute(data, "F_measure")))
    expect_equal(mean(test_E(x_prime)), summary(sm_compute(data, "E")))
})

test_that("two segments inside test values", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05 * 10))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p00 + 51,
                                              (p00 * 2) + 57))
    
    data <- sm_read(ref_sf, seg_sf)
    
    area_df <- get_areas(sm_ref(data), sm_seg(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- dplyr::bind_rows(dplyr::as_tibble(test_y_c(area_df)),
                             dplyr::as_tibble(test_y_d(area_df))) %>%
        dplyr::distinct(seg_id, ref_id, .keep_all = TRUE)
    
    expect_equal(mean(test_OS2(y_prime)), summary(sm_compute(data, "OS2")))
    expect_equal(mean(test_US2(y_prime)), summary(sm_compute(data, "US2")))
    expect_equal(mean(test_OS1(y_star)), summary(sm_compute(data, "OS1")))
    expect_equal(mean(test_US1(y_star)), summary(sm_compute(data, "US1")))
    expect_equal(mean(test_overMerging(y_star)), 
                 summary(sm_compute(data, "OMerging")))
    expect_equal(mean(test_underMerging(y_star)), 
                 summary(sm_compute(data, "UMerging")))
    expect_equal(mean(test_AFI(y_prime)), summary(sm_compute(data, "AFI")))
    expect_equal(mean(test_QR(y_star)), summary(sm_compute(data, "QR")))
    expect_equal(mean(test_D_index(y_star)), 
                 summary(sm_compute(data, "D_index")))
    expect_equal(mean(test_precision(x_prime)), 
                 summary(sm_compute(data, "precision")))
    expect_equal(mean(test_recall(y_prime)), 
                 summary(sm_compute(data, "recall")))
    expect_equal(mean(test_M(y_prime)), summary(sm_compute(data, "M")))
    expect_equal(mean(test_RAsub(y_tilde)), 
                 summary(sm_compute(data, "RAsub")))
    expect_equal(mean(test_RAsuper(y_tilde)), 
                 summary(sm_compute(data, "RAsuper")))
    expect_equal(mean(test_PI(y_tilde)), summary(sm_compute(data, "PI")))
    
    if (nrow(y_cd) == 0) {
        expect_true(sm_is_empty(sm_compute(data, "OS3")))
        expect_true(sm_is_empty(sm_compute(data, "US3")))
        expect_true(sm_is_empty(sm_compute(data, "ED3")))
    }
    expect_equal(test_F_measure(test_precision(x_prime), test_recall(y_prime)),
                 summary(sm_compute(data, "F_measure")))
    expect_equal(mean(test_E(x_prime)), summary(sm_compute(data, "E")))
})


test_that("grid test values", {
    # data test
    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5
    
    ref <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg <- sf::st_sf(geometry = sf::st_sfc(p05 + c(-0.5, -0.5),
                                           p05 + c( 0.5, -0.5),
                                           p05 + c( 0.5,  0.5),
                                           p05 + c(-0.5,  0.5)))
    data <- sm_read(ref, seg)
    
    area_df <- get_areas(sm_ref(data), sm_seg(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- dplyr::bind_rows(dplyr::as_tibble(test_y_c(area_df)),
                             dplyr::as_tibble(test_y_d(area_df))) %>%
        dplyr::distinct(seg_id, ref_id, .keep_all = TRUE)
    
    expect_equal(mean(test_OS2(y_prime)), summary(sm_compute(data, "OS2")))
    expect_equal(mean(test_US2(y_prime)), summary(sm_compute(data, "US2")))
    expect_equal(mean(test_OS1(y_star)), summary(sm_compute(data, "OS1")))
    expect_equal(mean(test_US1(y_star)), summary(sm_compute(data, "US1")))
    expect_equal(mean(test_overMerging(y_star)), 
                 summary(sm_compute(data, "OMerging")))
    expect_equal(mean(test_underMerging(y_star)), 
                 summary(sm_compute(data, "UMerging")))
    expect_equal(mean(test_AFI(y_prime)), summary(sm_compute(data, "AFI")))
    expect_equal(mean(test_QR(y_star)), summary(sm_compute(data, "QR")))
    expect_equal(mean(test_D_index(y_star)), 
                 summary(sm_compute(data, "D_index")))
    expect_equal(mean(test_precision(x_prime)), 
                 summary(sm_compute(data, "precision")))
    expect_equal(mean(test_recall(y_prime)), 
                 summary(sm_compute(data, "recall")))
    expect_equal(mean(test_M(y_prime)), summary(sm_compute(data, "M")))
    expect_equal(mean(test_RAsub(y_tilde)), 
                 summary(sm_compute(data, "RAsub")))
    expect_equal(mean(test_RAsuper(y_tilde)), 
                 summary(sm_compute(data, "RAsuper")))
    expect_equal(mean(test_PI(y_tilde)), summary(sm_compute(data, "PI")))
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_equal(
        test_F_measure(test_precision(x_prime), test_recall(y_prime)),
        summary(sm_compute(data, "F_measure")))
    expect_equal(mean(test_E(x_prime)), summary(sm_compute(data, "E")))
})


test_that("real test values", {
    # data test
    data("sample_ref_sf", package = "segmetric")
    data("sample_seg_sf", package = "segmetric")
    
    data <- sm_read(sample_ref_sf, sample_seg_sf)
    
    area_df <- get_areas(sm_ref(data), sm_seg(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- dplyr::bind_rows(dplyr::as_tibble(test_y_c(area_df)),
                             dplyr::as_tibble(test_y_d(area_df))) %>%
        dplyr::distinct(seg_id, ref_id, .keep_all = TRUE)
    
    expect_equal(test_OS2(y_prime), unlist(sm_compute(data, "OS2"), use.names = FALSE))
    expect_equal(test_US2(y_prime), unlist(sm_compute(data, "US2"), use.names = FALSE))
    expect_equal(test_OS1(y_star), unlist(sm_compute(data, "OS1"), use.names = FALSE))
    expect_equal(test_US1(y_star), unlist(sm_compute(data, "US1"), use.names = FALSE))
    expect_equal(test_overMerging(y_star), unlist(sm_compute(data, "OMerging"), use.names = FALSE))
    expect_equal(test_underMerging(y_star), unlist(sm_compute(data, "UMerging"), use.names = FALSE))
    expect_equal(test_AFI(y_prime), unlist(sm_compute(data, "AFI"), use.names = FALSE))
    expect_equal(test_QR(y_star), unlist(sm_compute(data, "QR"), use.names = FALSE))
    expect_equal(test_D_index(y_star), unlist(sm_compute(data, "D_index"), use.names = FALSE))
    expect_equal(test_precision(x_prime), unlist(sm_compute(data, "precision"), use.names = FALSE), tolerance = 2e-08)
    expect_equal(test_recall(y_prime), unlist(sm_compute(data, "recall"), use.names = FALSE), tolerance = 2e-08)
    expect_equal(test_M(y_prime), unlist(sm_compute(data, "M"), use.names = FALSE))
    expect_equal(test_RAsub(y_tilde), unlist(sm_compute(data, "RAsub"), use.names = FALSE))
    expect_equal(test_RAsuper(y_tilde), unlist(sm_compute(data, "RAsuper"), use.names = FALSE))
    expect_equal(test_PI(y_tilde), unlist(sm_compute(data, "PI"), use.names = FALSE))
    expect_equal(test_OS3(y_cd), unlist(sm_compute(data, "OS3"), use.names = FALSE))
    expect_equal(test_US3(y_cd), unlist(sm_compute(data, "US3"), use.names = FALSE))
    expect_equal(test_ED3(y_cd), unlist(sm_compute(data, "ED3"), use.names = FALSE))
    expect_equal(
        test_F_measure(test_precision(x_prime), test_recall(y_prime)),
        unlist(sm_compute(data, "F_measure"), use.names = FALSE))
    expect_equal(test_E(x_prime), unlist(sm_compute(data, "E"), use.names = FALSE), tolerance = 2e-05)
})
