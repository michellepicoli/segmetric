# TODO: make a plot function plot.metric (S3method)

# data test
p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
p05 <- p00 + 5

source("segmetric_util.R")


test_that("empty intersection", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1.01))
    data <- metric(ref_sf, seg_sf)
    
    expect_true(is.numeric(get_metric(data, "OS2")$OS2))
    expect_true(length(get_metric(data, "OS2")$OS2) == 0)
    
    expect_true(is.na(get_metric(data, "OS1")$OS1))
    
    expect_true(is.numeric(get_metric(data, "US2")$US2))
    expect_true(length(get_metric(data, "US2")$US2) == 0)
    
    expect_true(is.na(get_metric(data, "US1")))
    
    expect_true(is.numeric(get_metric(data, "AFI")$AFI))
    expect_true(length(get_metric(data, "AFI")$AFI) == 0)
    
    expect_true(is.numeric(get_metric(data, "QR")$QR))
    expect_true(length(get_metric(data, "QR")$QR) == 0)
    
    expect_true(is.na(get_metric(data, "D_index")))
    
    expect_true(is.nan(get_metric(data, "precision")$precision))
    
    expect_true(is.nan(get_metric(data, "recall")$recall))
   
    expect_true(is.numeric(get_metric(data, "M")$M))
    expect_true(length(get_metric(data, "M")$M) == 0)
    
    expect_true(is.numeric(get_metric(data, "E")$E))
    expect_true(length(get_metric(data, "E")$E) == 0)
    
    expect_true(is.numeric(get_metric(data, "RAsub")$RAsub))
    expect_true(length(get_metric(data, "RAsub")$RAsub) == 0)
    
    expect_true(is.numeric(get_metric(data, "RAsuper")$RAsuper))
    expect_true(length(get_metric(data, "RAsuper")$RAsuper) == 0)
    
    expect_true(is.numeric(get_metric(data, "PI")$PI))
    expect_true(length(get_metric(data, "PI")$PI) == 0)
    
    expect_true(is.na(get_metric(data, "OS3")$OS3))
    
    expect_true(is.na(get_metric(data, "US3")$US3))
    
    expect_true(is.na(get_metric(data, "ED3")$ED3))
    
    expect_true(is.nan(get_metric(data, "F_measure")$F_measure))
    
    expect_true(is.na(get_metric(data, "UMerging")$UMerging))
    
    expect_true(is.na(get_metric(data, "OMerging")$OMerging))
    
    expect_true(is.numeric(get_metric(data, "F")$F))
    expect_true(length(get_metric(data, "F")$F) == 0)
    
})



test_that("one vertex", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1))
    data <- metric(ref_sf, seg_sf)
    
    # TODO: Test.
    
})

test_that("one edge", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + c(0, 1)))
    data <- metric(ref_sf, seg_sf)
    
    # TODO: Test.
    
})

test_that("one vertex and a polygon", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1,
                                              p05 - 0.5))
    data <- metric(ref_sf, seg_sf)
    
    # TODO: Test.
    
})

test_that("normal use", {
    
    ref <- sf::st_sf(geometry = sf::st_sfc(p05 * 1))
    seg <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
                                           p05 - 0.5,
                                           p05 + c(0.8, -0.9),
                                           p05 + c(-0.8, 0.9)))
    data <- metric(ref, seg)
    
    expect_true(is.numeric(get_metric(data, "OS2")$OS2))
    expect_true(length(get_metric(data, "OS2")$OS2) == 1)
    expect_true(is.numeric(get_metric(data, "OS1")$OS1))
    expect_true(length(get_metric(data, "OS1")$OS1) == 1)
    expect_true(is.numeric(get_metric(data, "US2")$US2))
    expect_true(length(get_metric(data, "US2")$US2) == 1)
    expect_true(!is.na(get_metric(data, "US1")$US1))
    expect_true(length(get_metric(data, "US1")$US1) == 1)
    expect_true(is.numeric(get_metric(data, "AFI")$AFI))
    expect_true(length(get_metric(data, "AFI")$AFI) == 1)
    expect_true(is.numeric(get_metric(data, "QR")$QR))
    expect_true(length(get_metric(data, "QR")$QR) == 1)
    expect_true(!is.na(get_metric(data, "D_index")$D_index))
    expect_true(length(get_metric(data, "D_index")$D_index) == 1)
    expect_true(!is.nan(get_metric(data, "precision")$precision))
    expect_true(!is.na(get_metric(data, "precision")$precision))
    expect_true(is.numeric(get_metric(data, "precision")$precision))
    expect_true(length(get_metric(data, "precision")$precision) == 1)
    expect_true(!is.nan(get_metric(data, "recall")$recall))
    expect_true(!is.na(get_metric(data, "recall")$recall))
    expect_true(is.numeric(get_metric(data, "recall")$recall))
    expect_true(length(get_metric(data, "recall")$recall) == 1)
    expect_true(is.numeric(get_metric(data, "M")$M))
    expect_true(length(get_metric(data, "M")$M) == 1)
    expect_true(is.numeric(get_metric(data, "E")$E))
    expect_true(length(get_metric(data, "E")$E) == 4)
    expect_true(is.numeric(get_metric(data, "RAsub")$RAsub))
    expect_true(length(get_metric(data, "RAsub")$RAsub) == 4)
    expect_true(is.numeric(get_metric(data, "RAsuper")$RAsuper))
    expect_true(length(get_metric(data, "RAsuper")$RAsuper) == 4)
    expect_true(is.numeric(get_metric(data, "PI")$PI))
    expect_true(length(get_metric(data, "PI")$PI) == 4)
    expect_true(is.na(get_metric(data, "OS3")$OS3))
    expect_true(is.na(get_metric(data, "US3")$US3))
    expect_true(is.na(get_metric(data, "ED3")$ED3))
    expect_true(!is.nan(get_metric(data, "F_measure")$F_measure))
    expect_true(!is.na(get_metric(data, "F_measure")$F_measure))
    expect_true(length(get_metric(data, "F_measure")$F_measure) == 1)
    
    # TODO: Functions not found. 
    expect_true(is.numeric(get_metric(data, "UMerging")$UMerging))
    expect_true(length(get_metric(data, "UMerging")$UMerging) == 1)
    expect_true(is.numeric(get_metric(data, "OMerging")$OMerging))
    expect_true(length(get_metric(data, "OMerging")$OMerging) == 1)
    expect_true(is.numeric(get_metric(data, "F")$F))
    expect_true(length(get_metric(data, "F")$F) == 4)
    
    area_df <- get_areas(ref_sf(data), seg_sf(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- rbind(test_y_c(area_df), test_y_d(area_df))
    
    expect_true(test_OS2(y_prime) == get_metric(data, "OS2"))
    expect_true(test_US2(y_prime) == get_metric(data, "US2"))
    expect_true(test_OS1(y_star)  == get_metric(data, "OS1"))
    expect_true(test_US1(y_star)  == get_metric(data, "US1"))
    expect_true(test_overMerging(y_star)  == get_metric(data, "OMerging"))
    expect_true(test_underMerging(y_star) == get_metric(data, "UMerging"))
    expect_true(test_AFI(y_prime) == get_metric(data, "AFI"))
    # TODO: check why is not equal
    expect_true(test_QR(y_star) == get_metric(data, "QR"))
    expect_true(test_D_index(y_star) == get_metric(data, "D_index"))
    expect_true(test_precision(x_prime) == get_metric(data, "precision"))
    expect_true(test_recall(y_prime) == get_metric(data, "recall"))
    expect_true(test_M(y_prime) == get_metric(data, "M"))
    expect_true(all(test_RAsub(y_tilde) == unlist(get_metric(data, "RAsub"))))
    expect_true(all(test_RAsuper(y_tilde) == unlist(get_metric(data, "RAsuper"))))
    expect_true(all(test_PI(y_tilde) == unlist(get_metric(data, "PI"))))
    if (nrow(y_cd) == 0) {
        expect_true(is.na(get_metric(data, "OS3")))
        expect_true(is.na(get_metric(data, "US3")))
        expect_true(is.na(get_metric(data, "ED3")))
    } 
    expect_true(
        test_F_measure(test_precision(x_prime), 
                       test_recall(y_prime)) == get_metric(data, "F_measure"))
    
})



test_that("different CRS", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
                                              p05 - 0.5,
                                              p05 + c(0.8, -0.9),
                                              p05 + c(-0.8, 0.9)))
    data <- metric(ref_sf, seg_sf)
    
    # TODO: test.    
    
})

test_that("perfect fit", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    
    data <- metric(ref_sf, seg_sf)
    
    # TODO: test.    
    
})

test_that("two segments inside", {
    
    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05 * 10))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p00 + 51,
                                              (p00 * 2) + 57))
    
    data <- metric(ref_sf, seg_sf)
    
    # TODO: test.    
    
})

test_that("test grid", {
    
    ref <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg <- sf::st_sf(geometry = sf::st_sfc(p05 + c(-0.5, -0.5),
                                           p05 + c( 0.5, -0.5),
                                           p05 + c( 0.5,  0.5),
                                           p05 + c(-0.5,  0.5)))
    data <- metric(ref, seg)
    
    expect_true(is.numeric(get_metric(data, "OS2")$OS2))
    expect_true(length(get_metric(data, "OS2")$OS2) == 1)
    expect_true(is.numeric(get_metric(data, "OS1")$OS1))
    expect_true(length(get_metric(data, "OS1")$OS1) == 4)
    expect_true(is.numeric(get_metric(data, "US2")$US2))
    expect_true(length(get_metric(data, "US2")$US2) == 1)
    expect_true(is.numeric(get_metric(data, "US1")$US1))
    expect_true(length(get_metric(data, "US1")$US1) == 4)
    expect_true(length(get_metric(data, "US1")$US1) == 4)
    expect_true(is.numeric(get_metric(data, "AFI")$AFI))
    expect_true(length(get_metric(data, "AFI")$AFI) == 1)
    expect_true(is.numeric(get_metric(data, "QR")$QR))
    expect_true(length(get_metric(data, "QR")$QR) == 4)
    expect_true(is.numeric(get_metric(data, "D_index")$D_index))
    expect_true(length(get_metric(data, "D_index")$D_index) == 4)
    expect_true(is.numeric(get_metric(data, "D_index")$D_index))
    expect_true(length(get_metric(data, "D_index")$D_index) == 4)
    expect_true(!is.nan(get_metric(data, "precision")$precision))
    expect_true(!is.na(get_metric(data, "precision")$precision))
    expect_true(is.numeric(get_metric(data, "precision")$precision))
    expect_true(length(get_metric(data, "precision")$precision) == 1)
    expect_true(!is.nan(get_metric(data, "recall")$recall))
    expect_true(!is.na(get_metric(data, "recall")$recall))
    expect_true(is.numeric(get_metric(data, "recall")$recall))
    expect_true(length(get_metric(data, "recall")$recall) == 1)
    expect_true(is.numeric(get_metric(data, "M")$M))
    expect_true(length(get_metric(data, "M")$M) == 1)
    expect_true(is.numeric(get_metric(data, "E")$E))
    expect_true(length(get_metric(data, "E")$E) == 4)
    expect_true(is.numeric(get_metric(data, "RAsub")$RAsub))
    expect_true(length(get_metric(data, "RAsub")$RAsub) == 4)
    expect_true(is.numeric(get_metric(data, "RAsuper")$RAsuper))
    expect_true(length(get_metric(data, "RAsuper")$RAsuper) == 4)
    expect_true(is.numeric(get_metric(data, "PI")$PI))
    expect_true(length(get_metric(data, "PI")$PI) == 4)
    expect_true(is.na(get_metric(data, "OS3")$OS3))
    expect_true(is.na(get_metric(data, "US3")$US3))
    expect_true(is.na(get_metric(data, "ED3")$ED3))
    expect_true(!is.nan(get_metric(data, "F_measure")$F_measure))
    expect_true(!is.na(get_metric(data, "F_measure")$F_measure))
    expect_true(length(get_metric(data, "F_measure")$F_measure) == 1)

    area_df <- get_areas(ref_sf(data), seg_sf(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- rbind(test_y_c(area_df), test_y_d(area_df))
   
    expect_true(mean(test_OS2(y_prime)) == get_metric(data, "OS2"))
    expect_true(mean(test_US2(y_prime)) == get_metric(data, "US2"))
    expect_true(all(test_OS1(y_star)  == get_metric(data, "OS1")$OS1))
    expect_true(all(test_US1(y_star)  == get_metric(data, "US1")$US1))
    expect_true(all(test_overMerging(y_star)  == get_metric(data, "OMerging")$OMerging))
    expect_true(all(test_underMerging(y_star) == get_metric(data, "UMerging")$UMerging))
    expect_true(mean(test_AFI(y_prime)) == get_metric(data, "AFI")$AFI)
    expect_true(all(test_QR(y_star) == get_metric(data, "QR")$QR))
    expect_true(all(test_D_index(y_star) == get_metric(data, "D_index")$D_index))
    expect_true(test_precision(x_prime) == get_metric(data, "precision"))
    expect_true(test_recall(y_prime) == get_metric(data, "recall"))
    expect_true(mean(test_M(y_prime)) == get_metric(data, "M")$M)
    expect_true(all(test_RAsub(y_tilde) == unlist(get_metric(data, "RAsub"))))
    expect_true(all(test_RAsuper(y_tilde) == unlist(get_metric(data, "RAsuper"))))
    expect_true(all(test_PI(y_tilde) == unlist(get_metric(data, "PI"))))
    if (nrow(y_cd) == 0) {
        expect_true(is.na(get_metric(data, "OS3")))
        expect_true(is.na(get_metric(data, "US3")))
        expect_true(is.na(get_metric(data, "ED3")))
    } 
    expect_true(
        test_F_measure(test_precision(x_prime), 
                       test_recall(y_prime)) == get_metric(data, "F_measure"))
    
    
        
})
