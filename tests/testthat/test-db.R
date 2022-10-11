# TODO: Check if all the metrics are tested, every time!
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

    # TODO: This test was supposed to throw errors, but it doesn't because
    #       of the test data set. We need to prepare a better test data set.
    expect_equal(
        mean(test_F_measure(test_precision(x_prime), test_recall(y_prime))),
        summary(sm_compute(data, "F_measure"))
    )
    expect_equal(mean(test_E(x_prime)), summary(sm_compute(data, "E")))

    expect_equal(mean(test_IoU(y_prime)), summary(sm_compute(data, "IoU")))
    expect_equal(mean(test_SimSize(y_star)),
                 summary(sm_compute(data, "SimSize")))
    expect_equal(mean(test_qLoc(y_star)),
                 summary(sm_compute(data, "qLoc")))
    expect_equal(mean(test_RPsub(y_tilde)),
                 summary(sm_compute(data, "RPsub")))
    expect_equal(mean(test_RPsuper(y_star)),
                 summary(sm_compute(data, "RPsuper")))

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
    expect_equal(mean(test_IoU(y_prime)), summary(sm_compute(data, "IoU")))
    expect_equal(mean(test_SimSize(y_star)),
                 summary(sm_compute(data, "SimSize")))
    expect_equal(mean(test_qLoc(y_star)),
                 summary(sm_compute(data, "qLoc")))
    expect_equal(mean(test_RPsub(y_tilde)),
                 summary(sm_compute(data, "RPsub")))
    expect_equal(mean(test_RPsuper(y_star)),
                 summary(sm_compute(data, "RPsuper")))

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
    expect_equal(test_PI(y_tilde), summary(sm_compute(data, "PI")))

    if (nrow(y_cd) == 0) {
        expect_true(sm_is_empty(sm_compute(data, "OS3")))
        expect_true(sm_is_empty(sm_compute(data, "US3")))
        expect_true(sm_is_empty(sm_compute(data, "ED3")))
    }
    expect_equal(test_F_measure(test_precision(x_prime), test_recall(y_prime)),
                 summary(sm_compute(data, "F_measure")))
    expect_equal(mean(test_E(x_prime)), summary(sm_compute(data, "E")))
    expect_equal(mean(test_IoU(y_prime)), summary(sm_compute(data, "IoU")))
    expect_equal(mean(test_SimSize(y_star)),
                 summary(sm_compute(data, "SimSize")))
    expect_equal(mean(test_qLoc(y_star)),
                 summary(sm_compute(data, "qLoc")))
    expect_equal(mean(test_RPsub(y_tilde)),
                 summary(sm_compute(data, "RPsub")))
    expect_equal(mean(test_RPsuper(y_star)),
                 summary(sm_compute(data, "RPsuper")))

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
    expect_equal(mean(test_IoU(y_prime)), summary(sm_compute(data, "IoU")))
    expect_equal(mean(test_SimSize(y_star)),
                 summary(sm_compute(data, "SimSize")))
    expect_equal(mean(test_qLoc(y_star)),
                 summary(sm_compute(data, "qLoc")))
    expect_equal(mean(test_RPsub(y_tilde)),
                 summary(sm_compute(data, "RPsub")))
    expect_equal(mean(test_RPsuper(y_star)),
                 summary(sm_compute(data, "RPsuper")))

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

    expect_equal(
        test_OS2(y_prime), unlist(sm_compute(data, "OS2"), use.names = FALSE)
    )
    expect_equal(
        test_US2(y_prime), unlist(sm_compute(data, "US2"), use.names = FALSE)
    )
    expect_equal(
        test_OS1(y_star), unlist(sm_compute(data, "OS1"), use.names = FALSE)
    )
    expect_equal(
        test_US1(y_star), unlist(sm_compute(data, "US1"), use.names = FALSE)
    )
    expect_equal(
        test_overMerging(y_star), unlist(sm_compute(data, "OMerging"),
                                         use.names = FALSE)
    )
    expect_equal(
        test_underMerging(y_star), unlist(sm_compute(data, "UMerging"),
                                          use.names = FALSE)
    )
    expect_equal(
        test_AFI(y_prime), unlist(sm_compute(data, "AFI"), use.names = FALSE)
    )
    expect_equal(
        test_QR(y_star), unlist(sm_compute(data, "QR"), use.names = FALSE)
    )
    expect_equal(
        test_D_index(y_star), unlist(sm_compute(data, "D_index"),
                                     use.names = FALSE)
    )
    expect_equal(
        test_precision(x_prime), unlist(sm_compute(data, "precision"),
                                        use.names = FALSE),
                 tolerance = 2e-08
    )
    expect_equal(
        test_recall(y_prime), unlist(sm_compute(data, "recall"),
                                     use.names = FALSE),
                 tolerance = 2e-08
    )
    expect_equal(
        test_M(y_prime), unlist(sm_compute(data, "M"), use.names = FALSE)
    )
    expect_equal(
        test_RAsub(y_tilde), unlist(sm_compute(data, "RAsub"),
                                    use.names = FALSE)
    )
    expect_equal(
        test_RAsuper(y_tilde), unlist(sm_compute(data, "RAsuper"),
                                      use.names = FALSE)
    )
    expect_equal(
        test_PI(y_tilde), unlist(sm_compute(data, "PI"), use.names = FALSE)
    )
    expect_equal(
        test_OS3(y_cd), unlist(sm_compute(data, "OS3"), use.names = FALSE)
    )
    expect_equal(
        test_US3(y_cd), unlist(sm_compute(data, "US3"), use.names = FALSE)
    )
    expect_equal(
        test_ED3(y_cd), unlist(sm_compute(data, "ED3"), use.names = FALSE)
    )
    expect_equal(
        test_F_measure(test_precision(x_prime), test_recall(y_prime)),
        unlist(sm_compute(data, "F_measure"), use.names = FALSE)
    )
    expect_equal(
        test_E(x_prime), unlist(sm_compute(data, "E"), use.names = FALSE),
        tolerance = 2e-05
    )
    expect_equal(mean(test_IoU(y_prime)), summary(sm_compute(data, "IoU")))
    expect_equal(mean(test_SimSize(y_star)),
                 summary(sm_compute(data, "SimSize")))
    expect_equal(mean(test_qLoc(y_star)),
                 summary(sm_compute(data, "qLoc")))
    expect_equal(mean(test_RPsub(y_tilde)),
                 summary(sm_compute(data, "RPsub")))
    expect_equal(mean(test_RPsuper(y_star)),
                 summary(sm_compute(data, "RPsuper")))

})



test_that("perfect fit produces optimal value", {

    p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
    p05 <- p00 + 5

    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05))

    data <- sm_read(ref_sf, seg_sf)

    tlr <- .Machine$double.eps^0.5

    expect_true(
        unlist(sm_compute(data, "OS2")) - .db_get("OS2")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "OS1")) - .db_get("OS1")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "US2")) - .db_get("US2")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "US1")) - .db_get("US1")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "AFI")) - .db_get("AFI")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "QR"))  - .db_get("QR")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "D_index")) - .db_get("D_index")$optimal <  tlr
    )
    expect_true(
    unlist(sm_compute(data, "precision")) - .db_get("precision")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "recall")) - .db_get("recall")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "M")) - .db_get("M")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "E")) - .db_get("E")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "RAsub")) - .db_get("RAsub")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "RAsuper")) - .db_get("RAsuper")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "PI")) - .db_get("PI")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "OS3")) - .db_get("OS3")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "US3")) - .db_get("US3")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "ED3")) - .db_get("ED3")$optimal <  tlr
    )
    expect_true(
    unlist(sm_compute(data, "F_measure")) - .db_get("F_measure")$optimal <  tlr
    )
    expect_true(
    unlist(sm_compute(data, "UMerging")) - .db_get("UMerging")$optimal <  tlr
    )
    expect_true(
    unlist(sm_compute(data, "OMerging")) - .db_get("OMerging")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "Fitness")) - .db_get("Fitness")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "IoU")) - .db_get("IoU")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "SimSize")) - .db_get("SimSize")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "qLoc")) - .db_get("qLoc")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "RPsub")) - .db_get("RPsub")$optimal <  tlr
    )
    expect_true(
        unlist(sm_compute(data, "RPsuper")) - .db_get("RPsuper")$optimal <  tlr
    )
})



test_that("test metric fall in valid range", {

    tolerance <- .Machine$double.eps^0.5


    #---- test that metrics are between 0 and 0.5 ----

    data("seg200_sf", package = "segmetric")
    data("seg1000_sf", package = "segmetric")
    data <- sm_read(seg200_sf, seg1000_sf)

    expect_true(
        all(c(unlist(sm_compute(data, "OMerging")) >= 0 - tolerance,
              unlist(sm_compute(data, "OMerging")) <= 0.5 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "UMerging")) >= 0 - tolerance,
              unlist(sm_compute(data, "UMerging")) <= 0.5 + tolerance))
    )



    #---- test that metrics are equal or less than 1 ----

    expect_true(
        all(unlist(sm_compute(data, "AFI")) <= 1 + tolerance)
    )



    #---- test that metrics are between 0 and 1 ----

    expect_true(
        all(c(unlist(sm_compute(data, "OS1")) >= 0 - tolerance,
              unlist(sm_compute(data, "OS1")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "US1")) >= 0 - tolerance,
              unlist(sm_compute(data, "US1")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "OS2")) >= 0 - tolerance,
              unlist(sm_compute(data, "OS2")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "US2")) >= 0 - tolerance,
              unlist(sm_compute(data, "US2")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "OS3")) >= 0 - tolerance,
              unlist(sm_compute(data, "OS3")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "US3")) >= 0 - tolerance,
              unlist(sm_compute(data, "US3")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "precision")) >= 0 - tolerance,
              unlist(sm_compute(data, "precision")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "recall")) >= 0 - tolerance,
              unlist(sm_compute(data, "recall")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "M")) >= 0 - tolerance,
              unlist(sm_compute(data, "M")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "RAsub")) >= 0 - tolerance,
              unlist(sm_compute(data, "RAsub")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "RAsuper")) >= 0 - tolerance,
              unlist(sm_compute(data, "RAsuper")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "PI")) >= 0 - tolerance,
              unlist(sm_compute(data, "PI")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "ED3")) >= 0 - tolerance,
              unlist(sm_compute(data, "ED3")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "F_measure")) >= 0 - tolerance,
              unlist(sm_compute(data, "F_measure")) <= 1 + tolerance))
    )
    expect_true(
        all(c(unlist(sm_compute(data, "QR")) >= 0 - tolerance,
              unlist(sm_compute(data, "QR")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "D_index")) >= 0 - tolerance,
              unlist(sm_compute(data, "D_index")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "IoU")) >= 0 - tolerance,
              unlist(sm_compute(data, "IoU")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "SimSize")) >= 0 - tolerance,
              unlist(sm_compute(data, "SimSize")) <= 1 + tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "RPsuper")) >= 0 - tolerance,
              unlist(sm_compute(data, "RPsuper")) <= 1 + tolerance))
    )



    #---- test that metrics are between 0 and 50 ----

    expect_true(
        all(c(unlist(sm_compute(data, "E")) >= 0 - tolerance,
              unlist(sm_compute(data, "E")) <= 50 + tolerance))
    )



    #---- test that metrics are greater or equal to 0 ----

    expect_true(
        all(c(unlist(sm_compute(data, "qLoc")) >= 0 - tolerance))
    )

    expect_true(
        all(c(unlist(sm_compute(data, "RPsub")) >= 0 - tolerance))
    )

})



