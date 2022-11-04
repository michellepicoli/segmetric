# TODO:
# - Check if all the metrics are tested, every time!
# - Check the summary function!
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
    expect_true(sm_is_empty(sm_compute(data, "IoU")))
    expect_true(sm_is_empty(sm_compute(data, "SimSize")))
    expect_true(sm_is_empty(sm_compute(data, "qLoc")))
    expect_true(sm_is_empty(sm_compute(data, "RPsub")))
    expect_true(sm_is_empty(sm_compute(data, "RPsuper")))
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
    expect_true(sm_is_empty(sm_compute(data, "IoU")))
    expect_true(sm_is_empty(sm_compute(data, "SimSize")))
    expect_true(sm_is_empty(sm_compute(data, "qLoc")))
    expect_true(sm_is_empty(sm_compute(data, "RPsub")))
    expect_true(sm_is_empty(sm_compute(data, "RPsuper")))
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
    expect_true(sm_is_empty(sm_compute(data, "IoU")))
    expect_true(sm_is_empty(sm_compute(data, "SimSize")))
    expect_true(sm_is_empty(sm_compute(data, "qLoc")))
    expect_true(sm_is_empty(sm_compute(data, "RPsub")))
    expect_true(sm_is_empty(sm_compute(data, "RPsuper")))
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
    expect_true(!sm_is_empty(sm_compute(data, "IoU")))
    expect_true(!sm_is_empty(sm_compute(data, "SimSize")))
    expect_true(!sm_is_empty(sm_compute(data, "qLoc")))
    expect_true(!sm_is_empty(sm_compute(data, "RPsub")))
    expect_true(!sm_is_empty(sm_compute(data, "RPsuper")))
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
    expect_true(!sm_is_empty(sm_compute(data, "IoU")))
    expect_true(!sm_is_empty(sm_compute(data, "SimSize")))
    expect_true(!sm_is_empty(sm_compute(data, "qLoc")))
    expect_true(!sm_is_empty(sm_compute(data, "RPsub")))
    expect_true(!sm_is_empty(sm_compute(data, "RPsuper")))
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
    expect_true(!sm_is_empty(sm_compute(data, "IoU")))
    expect_true(!sm_is_empty(sm_compute(data, "SimSize")))
    expect_true(!sm_is_empty(sm_compute(data, "qLoc")))
    expect_true(!sm_is_empty(sm_compute(data, "RPsub")))
    expect_true(!sm_is_empty(sm_compute(data, "RPsuper")))
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
    expect_true(!sm_is_empty(sm_compute(data, "UMerging")))
    expect_true(!sm_is_empty(sm_compute(data, "OMerging")))
    expect_true(!sm_is_empty(sm_compute(data, "Fitness")))
    expect_true(!sm_is_empty(sm_compute(data, "IoU")))
    expect_true(!sm_is_empty(sm_compute(data, "SimSize")))
    expect_true(!sm_is_empty(sm_compute(data, "qLoc")))
    expect_true(!sm_is_empty(sm_compute(data, "RPsub")))
    expect_true(!sm_is_empty(sm_compute(data, "RPsuper")))
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
    expect_true(!sm_is_empty(sm_compute(data, "UMerging")))
    expect_true(!sm_is_empty(sm_compute(data, "OMerging")))
    expect_true(!sm_is_empty(sm_compute(data, "Fitness")))
    expect_true(!sm_is_empty(sm_compute(data, "IoU")))
    expect_true(!sm_is_empty(sm_compute(data, "SimSize")))
    expect_true(!sm_is_empty(sm_compute(data, "qLoc")))
    expect_true(!sm_is_empty(sm_compute(data, "RPsub")))
    expect_true(!sm_is_empty(sm_compute(data, "RPsuper")))
})

test_that("normal use test values", {

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

    expect_equal(test_OS2(y_prime),         sm_compute(data, "OS2")$OS2)
    expect_equal(test_US2(y_prime),         sm_compute(data, "US2")$US2)
    expect_equal(test_OS1(y_star),          sm_compute(data, "OS1")$OS1)
    expect_equal(test_US1(y_star),          sm_compute(data, "US1")$US1)
    expect_equal(test_overMerging(y_star),  sm_compute(data, "OMerging")$OMerging)
    expect_equal(test_underMerging(y_star), sm_compute(data, "UMerging")$UMerging)
    expect_equal(test_AFI(y_prime),         sm_compute(data, "AFI")$AFI)
    expect_equal(test_QR(y_star),           sm_compute(data, "QR")$QR)
    expect_equal(test_D_index(y_star),      sm_compute(data, "D_index")$D_index)
    expect_equal(test_precision(x_prime),   sm_compute(data, "precision")$precision)
    expect_equal(test_recall(y_prime),      sm_compute(data, "recall")$recall)
    expect_equal(test_M(y_prime),           sm_compute(data, "M")$M)
    expect_equal(test_RAsub(y_tilde),       sm_compute(data, "RAsub")$RAsub)
    expect_equal(test_RAsuper(y_tilde),     sm_compute(data, "RAsuper")$RAsuper)
    expect_equal(test_PI(y_tilde),          sm_compute(data, "PI")$PI)
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_equal(test_F_measure(test_precision(x_prime), test_recall(y_prime)),
                                             sm_compute(data, "F_measure")$F_measure)
    expect_equal(test_E(x_prime),            sm_compute(data, "E")$E)
    expect_equal(test_IoU(y_prime),          sm_compute(data, "IoU")$IoU)
    expect_equal(test_SimSize(y_star),       sm_compute(data, "SimSize")$SimSize)
    expect_equal(test_qLoc(y_star),          sm_compute(data, "qLoc")$qLoc)
    expect_equal(test_RPsub(y_tilde),        sm_compute(data, "RPsub")$RPsub)
    expect_equal(test_RPsuper(y_star),       sm_compute(data, "RPsuper")$RPsuper)

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

    expect_equal(test_OS2(y_prime),         sm_compute(data, "OS2")$OS2)
    expect_equal(test_US2(y_prime),         sm_compute(data, "US2")$US2)
    expect_equal(test_OS1(y_star),          sm_compute(data, "OS1")$OS1)
    expect_equal(test_US1(y_star),          sm_compute(data, "US1")$US1)
    expect_equal(test_overMerging(y_star),  sm_compute(data, "OMerging")$OMerging)
    expect_equal(test_underMerging(y_star), sm_compute(data, "UMerging")$UMerging)
    expect_equal(test_AFI(y_prime),         sm_compute(data, "AFI")$AFI)
    expect_equal(test_QR(y_star),           sm_compute(data, "QR")$QR)
    expect_equal(test_D_index(y_star),      sm_compute(data, "D_index")$D_index)
    expect_equal(test_precision(x_prime),   sm_compute(data, "precision")$precision)
    expect_equal(test_recall(y_prime),      sm_compute(data, "recall")$recall)
    expect_equal(test_M(y_prime),           sm_compute(data, "M")$M)
    expect_equal(test_RAsub(y_tilde),       sm_compute(data, "RAsub")$RAsub)
    expect_equal(test_RAsuper(y_tilde),     sm_compute(data, "RAsuper")$RAsuper)
    expect_equal(test_PI(y_tilde),          sm_compute(data, "PI")$PI)
    expect_equal(test_OS3(y_cd),            sm_compute(data, "OS3")$OS3)
    expect_equal(test_US3(y_cd),            sm_compute(data, "US3")$US3)
    expect_equal(test_ED3(area_df),         sm_compute(data, "ED3")$ED3)
    expect_equal(test_F_measure(test_precision(x_prime), test_recall(y_prime)),
                                            sm_compute(data, "F_measure")$F_measure)
    expect_equal(test_E(x_prime),           sm_compute(data, "E")$E)
    expect_equal(test_IoU(y_prime),         sm_compute(data, "IoU")$IoU)
    expect_equal(test_SimSize(y_star),      sm_compute(data, "SimSize")$SimSize)
    expect_equal(test_qLoc(y_star),         sm_compute(data, "qLoc")$qLoc)
    expect_equal(test_RPsub(y_tilde),       sm_compute(data, "RPsub")$RPsub)
    expect_equal(test_RPsuper(y_star),      sm_compute(data, "RPsuper")$RPsuper)

})

test_that("two segments inside test values", {

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

    expect_equal(test_OS2(y_prime),         sm_compute(data, "OS2")$OS2)
    expect_equal(test_US2(y_prime),         sm_compute(data, "US2")$US2)
    expect_equal(test_OS1(y_star),          sm_compute(data, "OS1")$OS1)
    expect_equal(test_US1(y_star),          sm_compute(data, "US1")$US1)
    expect_equal(test_overMerging(y_star),  sm_compute(data, "OMerging")$OMerging)
    expect_equal(test_underMerging(y_star), sm_compute(data, "UMerging")$UMerging)
    expect_equal(test_AFI(y_prime),         sm_compute(data, "AFI")$AFI)
    expect_equal(test_QR(y_star),           sm_compute(data, "QR")$QR)
    expect_equal(test_D_index(y_star),      sm_compute(data, "D_index")$D_index)
    expect_equal(test_precision(x_prime),   sm_compute(data, "precision")$precision)
    expect_equal(test_recall(y_prime),      sm_compute(data, "recall")$recall)
    expect_equal(test_M(y_prime),           sm_compute(data, "M")$M)
    expect_equal(test_RAsub(y_tilde),       sm_compute(data, "RAsub")$RAsub)
    expect_equal(test_RAsuper(y_tilde),     sm_compute(data, "RAsuper")$RAsuper)
    expect_equal(test_PI(y_tilde),          sm_compute(data, "PI")$PI)
    if (nrow(y_cd) == 0) {
        expect_true(sm_is_empty(sm_compute(data, "OS3")))
        expect_true(sm_is_empty(sm_compute(data, "US3")))
        expect_true(sm_is_empty(sm_compute(data, "ED3")))
    }
    expect_equal(test_F_measure(test_precision(x_prime), test_recall(y_prime)),
                                             sm_compute(data, "F_measure")$F_measure)
    expect_equal(test_E(x_prime),            sm_compute(data, "E")$E)
    expect_equal(test_IoU(y_prime),          sm_compute(data, "IoU")$IoU)
    expect_equal(test_SimSize(y_star),       sm_compute(data, "SimSize")$SimSize)
    expect_equal(test_qLoc(y_star),          sm_compute(data, "qLoc")$qLoc)
    expect_equal(test_RPsub(y_tilde),        sm_compute(data, "RPsub")$RPsub)
    expect_equal(test_RPsuper(y_star),       sm_compute(data, "RPsuper")$RPsuper)

})


test_that("grid test values", {

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

    expect_equal(test_OS2(y_prime),         sm_compute(data, "OS2")$OS2)
    expect_equal(test_US2(y_prime),         sm_compute(data, "US2")$US2)
    expect_equal(test_OS1(y_star),          sm_compute(data, "OS1")$OS1)
    expect_equal(test_US1(y_star),          sm_compute(data, "US1")$US1)
    expect_equal(test_overMerging(y_star),  sm_compute(data, "OMerging")$OMerging)
    expect_equal(test_underMerging(y_star), sm_compute(data, "UMerging")$UMerging)
    expect_equal(test_AFI(y_prime),         sm_compute(data, "AFI")$AFI)
    expect_equal(test_QR(y_star),           sm_compute(data, "QR")$QR)
    expect_equal(test_D_index(y_star),      sm_compute(data, "D_index")$D_index)
    expect_equal(test_precision(x_prime),   sm_compute(data, "precision")$precision)
    expect_equal(test_recall(y_prime),      sm_compute(data, "recall")$recall)
    expect_equal(test_M(y_prime),           sm_compute(data, "M")$M)
    expect_equal(test_RAsub(y_tilde),       sm_compute(data, "RAsub")$RAsub)
    expect_equal(test_RAsuper(y_tilde),     sm_compute(data, "RAsuper")$RAsuper)
    expect_equal(test_PI(y_tilde),          sm_compute(data, "PI")$PI)
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_equal( test_F_measure(test_precision(x_prime), test_recall(y_prime)),
                                            sm_compute(data, "F_measure")$F_measure)
    expect_equal(test_E(x_prime),           sm_compute(data, "E")$E)
    expect_equal(test_IoU(y_prime),         sm_compute(data, "IoU")$IoU)
    expect_equal(test_SimSize(y_star),      sm_compute(data, "SimSize")$SimSize)
    expect_equal(test_qLoc(y_star),         sm_compute(data, "qLoc")$qLoc)
    expect_equal(test_RPsub(y_tilde),       sm_compute(data, "RPsub")$RPsub)
    expect_equal(test_RPsuper(y_star),      sm_compute(data, "RPsuper")$RPsuper)

})


test_that("real test values", {

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

    expect_equal(test_OS2(y_prime),         sm_compute(data, "OS2")$OS2)
    expect_equal(test_US2(y_prime),         sm_compute(data, "US2")$US2)
    expect_equal(test_OS1(y_star),          sm_compute(data, "OS1")$OS1)
    expect_equal(test_US1(y_star),          sm_compute(data, "US1")$US1)
    expect_equal(test_overMerging(y_star),  sm_compute(data, "OMerging")$OMerging)
    expect_equal(test_underMerging(y_star), sm_compute(data, "UMerging")$UMerging)
    expect_equal(test_AFI(y_prime),         sm_compute(data, "AFI")$AFI)
    expect_equal(test_QR(y_star),           sm_compute(data, "QR")$QR)
    expect_equal(test_D_index(y_star),      sm_compute(data, "D_index")$D_index)
    expect_equal(test_precision(x_prime),   sm_compute(data, "precision")$precision, tolerance = 2e-08)
    expect_equal(test_recall(y_prime),      sm_compute(data, "recall")$recall, tolerance = 2e-08)
    expect_equal(test_M(y_prime),           sm_compute(data, "M")$M)
    expect_equal(test_RAsub(y_tilde),       sm_compute(data, "RAsub")$RAsub)
    expect_equal(test_RAsuper(y_tilde),     sm_compute(data, "RAsuper")$RAsuper)
    expect_equal(test_PI(y_tilde),          sm_compute(data, "PI")$PI)
    expect_equal(test_OS3(y_cd),            sm_compute(data, "OS3")$OS3)
    expect_equal(test_US3(y_cd),            sm_compute(data, "US3")$US3)
    expect_equal(test_ED3(y_cd),            sm_compute(data, "ED3")$ED3)
    expect_equal(test_F_measure(test_precision(x_prime), test_recall(y_prime)),
                                            sm_compute(data, "F_measure")$F_measure)
    expect_equal(test_E(x_prime),           sm_compute(data, "E")$E, tolerance = 2e-05)
    expect_equal(test_IoU(y_prime),         sm_compute(data, "IoU")$IoU)
    expect_equal(test_SimSize(y_star),      sm_compute(data, "SimSize")$SimSize)
    expect_equal(test_qLoc(y_star),         sm_compute(data, "qLoc")$qLoc)
    expect_equal(test_RPsub(y_tilde),       sm_compute(data, "RPsub")$RPsub)
    expect_equal(test_RPsuper(y_star),      sm_compute(data, "RPsuper")$RPsuper)

})



test_that("perfect fit produces optimal value", {

    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5

    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05))

    data <- sm_read(ref_sf, seg_sf)

    tlr <- .Machine$double.eps^0.5

    expect_true(sm_compute(data, "OS2")$OS2 - .db_get("OS2")$optimal <  tlr)
    expect_true(sm_compute(data, "OS1")$OS1 - .db_get("OS1")$optimal <  tlr)
    expect_true(sm_compute(data, "US2")$US2 - .db_get("US2")$optimal <  tlr)
    expect_true(sm_compute(data, "US1")$US1 - .db_get("US1")$optimal <  tlr)
    expect_true(sm_compute(data, "AFI")$AFI - .db_get("AFI")$optimal <  tlr)
    expect_true(sm_compute(data, "QR")$QR   - .db_get("QR")$optimal <  tlr)
    expect_true(sm_compute(data, "D_index")$D_index     - .db_get("D_index")$optimal <  tlr)
    expect_true(sm_compute(data, "precision")$precision - .db_get("precision")$optimal <  tlr)
    expect_true(sm_compute(data, "recall")$recall       - .db_get("recall")$optimal <  tlr)
    expect_true(sm_compute(data, "M")$M - .db_get("M")$optimal <  tlr)
    expect_true(sm_compute(data, "E")$E - .db_get("E")$optimal <  tlr)
    expect_true(sm_compute(data, "RAsub")$RAsub     - .db_get("RAsub")$optimal <  tlr)
    expect_true(sm_compute(data, "RAsuper")$RAsuper - .db_get("RAsuper")$optimal <  tlr)
    expect_true(sm_compute(data, "PI")$PI   - .db_get("PI")$optimal <  tlr)
    expect_true(sm_compute(data, "OS3")$OS3 - .db_get("OS3")$optimal <  tlr)
    expect_true(sm_compute(data, "US3")$US3 - .db_get("US3")$optimal <  tlr)
    expect_true(sm_compute(data, "ED3")$ED3 - .db_get("ED3")$optimal <  tlr)
    expect_true(sm_compute(data, "F_measure")$F_measure - .db_get("F_measure")$optimal <  tlr)
    expect_true(sm_compute(data, "UMerging")$UMerging   - .db_get("UMerging")$optimal <  tlr)
    expect_true(sm_compute(data, "OMerging")$OMerging   - .db_get("OMerging")$optimal <  tlr)
    expect_true(sm_compute(data, "Fitness")$Fitness - .db_get("Fitness")$optimal <  tlr)
    expect_true(sm_compute(data, "IoU")$IoU         - .db_get("IoU")$optimal <  tlr)
    expect_true(sm_compute(data, "SimSize")$SimSize - .db_get("SimSize")$optimal <  tlr)
    expect_true(sm_compute(data, "qLoc")$qLoc       - .db_get("qLoc")$optimal <  tlr)
    expect_true(sm_compute(data, "RPsub")$RPsub     - .db_get("RPsub")$optimal <  tlr)
    expect_true(sm_compute(data, "RPsuper")$RPsuper - .db_get("RPsuper")$optimal <  tlr)

})



test_that("test metric falls in range", {

    tlr <- .Machine$double.eps ^ 0.5

    data("seg200_sf", package = "segmetric")
    data("seg1000_sf", package = "segmetric")
    data <- sm_read(seg200_sf, seg1000_sf)
    m <- sm_compute(data, c("OMerging", "UMerging", "AFI", "OS1", "US1",
                            "OS2", "US2", "US3", "US3", "precision",
                            "recall", "M", "RAsub", "RAsuper", "PI", "ED3",
                            "F_measure", "QR", "D_index", "IoU", "SimSize",
                            "RPsuper", "E", "qLoc", "RPsub"))



    #---- test that metrics are between 0 and 0.5 ----

    expect_true(all(m$OMerging >= 0   - tlr))
    expect_true(all(m$OMerging <= 0.5 + tlr)) # NOTE: Check m$OMerging[m$OMerging > 0.5 + tlr]

    expect_true(all(m$UMerging >= 0   - tlr))
    expect_true(all(m$UMerging <= 0.5 + tlr)) # NOTE: Check m$UMerging[m$UMerging > 0.5 + tlr]



    #---- test that metrics are equal or less than 1 ----

    expect_true(all(m$AFI <= 1 + tlr))



    #---- test that metrics are between 0 and 1 ----

    expect_true(all(m$OS1 >= 0 - tlr))
    expect_true(all(m$OS1 <= 1 + tlr))

    expect_true(all(m$US1 >= 0 - tlr))
    expect_true(all(m$US1 <= 1 + tlr))

    expect_true(all(m$OS2 >= 0 - tlr))
    expect_true(all(m$OS2 <= 1 + tlr))

    expect_true(all(m$US2 >= 0 - tlr))
    expect_true(all(m$US2 <= 1 + tlr))

    expect_true(all(m$OS3 >= 0 - tlr))
    expect_true(all(m$OS3 <= 1 + tlr))

    expect_true(all(m$US3 >= 0 - tlr))
    expect_true(all(m$US3 <= 1 + tlr))

    expect_true(all(m$precision >= 0 - tlr))
    expect_true(all(m$precision <= 1 + tlr))

    expect_true(all(m$recall >= 0 - tlr))
    expect_true(all(m$recall <= 1 + tlr))

    expect_true(all(m$M >= 0 - tlr))
    expect_true(all(m$M <= 1 + tlr))

    expect_true(all(m$RAsub >= 0 - tlr))
    expect_true(all(m$RAsub <= 1 + tlr))

    expect_true(all(m$RAsuper >= 0 - tlr))
    expect_true(all(m$RAsuper <= 1 + tlr))

    expect_true(all(m$PI >= 0 - tlr))
    expect_true(all(m$PI <= 1 + tlr)) # NOTE: Check m$PI[m$PI > 1 + tlr]

    expect_true(all(m$ED3 >= 0 - tlr))
    expect_true(all(m$ED3 <= 1 + tlr))

    expect_true(all(m$F_measure >= 0 - tlr))
    expect_true(all(m$F_measure <= 1 + tlr))

    expect_true(all(m$QR >= 0 - tlr))
    expect_true(all(m$QR <= 1 + tlr))

    expect_true(all(m$D_index >= 0 - tlr))
    expect_true(all(m$D_index <= 1 + tlr))

    expect_true(all(m$IoU >= 0 - tlr))
    expect_true(all(m$IoU <= 1 + tlr))

    expect_true(all(m$SimSize >= 0 - tlr))
    expect_true(all(m$SimSize <= 1 + tlr))

    expect_true(all(m$RPsuper >= 0 - tlr))
    expect_true(all(m$RPsuper <= 1 + tlr))



    #---- test that metrics are between 0 and 50 ----

    expect_true(all(m$E >= 0  - tlr))
    expect_true(all(m$E <= 50 + tlr)) # NOTE: Check m$E[m$E > 50 + tlr]



    #---- test that metrics are greater or equal to 0 ----

    expect_true(all(m$qLoc >= 0  - tlr))

    expect_true(all(m$RPsub >= 0  - tlr))

})

