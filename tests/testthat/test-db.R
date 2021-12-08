# data test
p00 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
p05 <- p00 + 5

source("segmetric_util.R")



test_that("empty intersection works", {

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

    expect_true(sm_is_empty(sm_compute(data, "F")))

})



test_that("one vertex intersection works", {

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

    expect_true(sm_is_empty(sm_compute(data, "F")))
})

test_that("one edge intersection works", {

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

    expect_true(sm_is_empty(sm_compute(data, "F")))

})

test_that("one vertex and one polygon work", {

    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 1,
                                              p05 - 0.5))
    data <- sm_read(ref_sf, seg_sf)

    expect_true(is.numeric(sm_compute(data, "OS2")$OS2))
    expect_true(length(sm_compute(data, "OS2")$OS2) == 1)

    expect_true(!is.na(sm_compute(data, "OS1")$OS1))

    expect_true(is.numeric(sm_compute(data, "US2")$US2))
    expect_true(length(sm_compute(data, "US2")$US2) == 1)

    expect_true(!is.na(sm_compute(data, "US1")))

    expect_true(is.numeric(sm_compute(data, "AFI")$AFI))
    expect_true(length(sm_compute(data, "AFI")$AFI) == 1)

    expect_true(is.numeric(sm_compute(data, "QR")$QR))
    expect_true(length(sm_compute(data, "QR")$QR) == 1)

    expect_true(!is.na(sm_compute(data, "D_index")))

    expect_true(!is.nan(sm_compute(data, "precision")$precision))

    expect_true(!is.nan(sm_compute(data, "recall")$recall))

    expect_true(is.numeric(sm_compute(data, "M")$M))
    expect_true(length(sm_compute(data, "M")$M) == 1)

    expect_true(is.numeric(sm_compute(data, "E")$E))
    expect_true(length(sm_compute(data, "E")$E) == 1)

    expect_true(is.numeric(sm_compute(data, "RAsub")$RAsub))
    expect_true(length(sm_compute(data, "RAsub")$RAsub) == 1)

    expect_true(is.numeric(sm_compute(data, "RAsuper")$RAsuper))
    expect_true(length(sm_compute(data, "RAsuper")$RAsuper) == 1)

    expect_true(is.numeric(sm_compute(data, "PI")$PI))
    expect_true(length(sm_compute(data, "PI")$PI) == 1)

    expect_true(sm_is_empty(sm_compute(data, "OS3")))

    expect_true(sm_is_empty(sm_compute(data, "US3")))

    expect_true(sm_is_empty(sm_compute(data, "ED3")))

    expect_true(!sm_is_empty(sm_compute(data, "F_measure")))

    expect_true(!sm_is_empty(sm_compute(data, "UMerging")))

    expect_true(!sm_is_empty(sm_compute(data, "OMerging")))

    expect_true(!sm_is_empty(sm_compute(data, "F")))
})



test_that("normal use works", {

    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05 * 1))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05 + 0.9,
                                           p05 - 0.5,
                                           p05 + c(0.8, -0.9),
                                           p05 + c(-0.8, 0.9)))
    data <- sm_read(ref_sf, seg_sf)

    expect_true(is.numeric(sm_compute(data, "OS2")$OS2))
    expect_true(length(sm_compute(data, "OS2")$OS2) == 1)

    expect_true(is.numeric(sm_compute(data, "OS1")$OS1))
    expect_true(length(sm_compute(data, "OS1")$OS1) == 1)

    expect_true(is.numeric(sm_compute(data, "US2")$US2))
    expect_true(length(sm_compute(data, "US2")$US2) == 1)

    expect_true(!sm_is_empty(sm_compute(data, "US1")))
    expect_true(length(sm_compute(data, "US1")$US1) == 1)

    expect_true(is.numeric(sm_compute(data, "AFI")$AFI))
    expect_true(length(sm_compute(data, "AFI")$AFI) == 1)

    expect_true(is.numeric(sm_compute(data, "QR")$QR))
    expect_true(length(sm_compute(data, "QR")$QR) == 1)

    expect_true(!sm_is_empty(sm_compute(data, "D_index")))
    expect_true(length(sm_compute(data, "D_index")$D_index) == 1)

    expect_true(!sm_is_empty(sm_compute(data, "precision")))
    expect_true(is.numeric(sm_compute(data, "precision")$precision))
    expect_true(length(sm_compute(data, "precision")$precision) == 1)

    expect_true(!sm_is_empty(sm_compute(data, "recall")))
    expect_true(is.numeric(sm_compute(data, "recall")$recall))
    expect_true(length(sm_compute(data, "recall")$recall) == 1)

    expect_true(is.numeric(sm_compute(data, "M")$M))
    expect_true(length(sm_compute(data, "M")$M) == 1)

    expect_true(is.numeric(sm_compute(data, "E")$E))
    expect_true(length(sm_compute(data, "E")$E) == 4)

    expect_true(is.numeric(sm_compute(data, "RAsub")$RAsub))
    expect_true(length(sm_compute(data, "RAsub")$RAsub) == 4)

    expect_true(is.numeric(sm_compute(data, "RAsuper")$RAsuper))
    expect_true(length(sm_compute(data, "RAsuper")$RAsuper) == 4)

    expect_true(is.numeric(sm_compute(data, "PI")$PI))
    expect_true(length(sm_compute(data, "PI")$PI) == 4)

    expect_true(sm_is_empty(sm_compute(data, "OS3")))

    expect_true(sm_is_empty(sm_compute(data, "US3")))

    expect_true(sm_is_empty(sm_compute(data, "ED3")))

    expect_true(!sm_is_empty(sm_compute(data, "F_measure")))
    expect_true(length(sm_compute(data, "F_measure")$F_measure) == 1)

    expect_true(is.numeric(sm_compute(data, "UMerging")$UMerging))
    expect_true(length(sm_compute(data, "UMerging")$UMerging) == 1)

    expect_true(is.numeric(sm_compute(data, "OMerging")$OMerging))
    expect_true(length(sm_compute(data, "OMerging")$OMerging) == 1)

    expect_true(is.numeric(sm_compute(data, "F")$F))
    expect_true(length(sm_compute(data, "F")$F) == 4)

    area_df <- get_areas(sm_ref(data), sm_seg(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- dplyr::bind_rows(dplyr::as_tibble(test_y_c(area_df)),
                             dplyr::as_tibble(test_y_d(area_df))) %>%
        dplyr::distinct(seg_id, ref_id, .keep_all = TRUE)

    expect_true(test_OS2(y_prime) == sm_compute(data, "OS2")$OS2)
    expect_true(test_US2(y_prime) == sm_compute(data, "US2")$US2)
    expect_true(test_OS1(y_star)  == sm_compute(data, "OS1")$OS1)
    expect_true(test_US1(y_star)  == sm_compute(data, "US1")$US1)
    expect_true(test_overMerging(y_star)  == sm_compute(data, "OMerging")$OMerging)
    expect_true(test_underMerging(y_star) == sm_compute(data, "UMerging")$UMerging)
    expect_true(test_AFI(y_prime) == sm_compute(data, "AFI")$AFI)
    expect_true(test_QR(y_star) == sm_compute(data, "QR")$QR)
    expect_equal(test_D_index(y_star), sm_compute(data, "D_index")$D_index)
    expect_true(test_precision(x_prime) == sm_compute(data, "precision")$precision)
    expect_true(test_recall(y_prime) == sm_compute(data, "recall")$recall)
    expect_true(test_M(y_prime) == sm_compute(data, "M")$M)
    expect_true(all(test_RAsub(y_tilde) == sm_compute(data, "RAsub")$RAsub))
    expect_true(all(test_RAsuper(y_tilde) == sm_compute(data, "RAsuper")$RAsuper))
    expect_true(all(test_PI(y_tilde) == sm_compute(data, "PI")$PI))
    # if (nrow(y_cd) == 0) {
        expect_true(sm_is_empty(sm_compute(data, "OS3")))
        expect_true(sm_is_empty(sm_compute(data, "US3")))
        expect_true(sm_is_empty(sm_compute(data, "ED3")))
    # }
    expect_true(
        test_F_measure(test_precision(x_prime),
                       test_recall(y_prime)) == sm_compute(data, "F_measure")
    )

})



test_that("different CRS work", {

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



test_that("perfect fit works", {

    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p05))

    data <- sm_read(ref_sf, seg_sf)

    expect_true(is.numeric(sm_compute(data, "OS2")$OS2))
    expect_true(length(sm_compute(data, "OS2")$OS2) == 1)
    expect_true(is.numeric(sm_compute(data, "OS1")$OS1))
    expect_true(length(sm_compute(data, "OS1")$OS1) == 1)
    expect_true(is.numeric(sm_compute(data, "US2")$US2))
    expect_true(length(sm_compute(data, "US2")$US2) == 1)
    expect_true(!is.na(sm_compute(data, "US1")$US1))
    expect_true(length(sm_compute(data, "US1")$US1) == 1)
    expect_true(is.numeric(sm_compute(data, "AFI")$AFI))
    expect_true(length(sm_compute(data, "AFI")$AFI) == 1)
    expect_true(is.numeric(sm_compute(data, "QR")$QR))
    expect_true(length(sm_compute(data, "QR")$QR) == 1)
    expect_true(!is.na(sm_compute(data, "D_index")$D_index))
    expect_true(length(sm_compute(data, "D_index")$D_index) == 1)
    expect_true(!is.nan(sm_compute(data, "precision")$precision))
    expect_true(!is.na(sm_compute(data, "precision")$precision))
    expect_true(is.numeric(sm_compute(data, "precision")$precision))
    expect_true(length(sm_compute(data, "precision")$precision) == 1)
    expect_true(!is.nan(sm_compute(data, "recall")$recall))
    expect_true(!is.na(sm_compute(data, "recall")$recall))
    expect_true(is.numeric(sm_compute(data, "recall")$recall))
    expect_true(length(sm_compute(data, "recall")$recall) == 1)
    expect_true(is.numeric(sm_compute(data, "M")$M))
    expect_true(length(sm_compute(data, "M")$M) == 1)
    expect_true(is.numeric(sm_compute(data, "E")$E))
    expect_true(length(sm_compute(data, "E")$E) == 1)
    expect_true(is.numeric(sm_compute(data, "RAsub")$RAsub))
    expect_true(length(sm_compute(data, "RAsub")$RAsub) == 1)
    expect_true(is.numeric(sm_compute(data, "RAsuper")$RAsuper))
    expect_true(length(sm_compute(data, "RAsuper")$RAsuper) == 1)
    expect_true(is.numeric(sm_compute(data, "PI")$PI))
    expect_true(length(sm_compute(data, "PI")$PI) == 1)
    expect_true(!is.na(sm_compute(data, "OS3")$OS3))
    expect_true(!is.na(sm_compute(data, "US3")$US3))
    expect_true(!is.na(sm_compute(data, "ED3")$ED3))
    expect_true(!is.nan(sm_compute(data, "F_measure")$F_measure))
    expect_true(!is.na(sm_compute(data, "F_measure")$F_measure))
    expect_true(length(sm_compute(data, "F_measure")$F_measure) == 1)
    expect_true(is.numeric(sm_compute(data, "UMerging")$UMerging))
    expect_true(length(sm_compute(data, "UMerging")$UMerging) == 1)
    expect_true(is.numeric(sm_compute(data, "OMerging")$OMerging))
    expect_true(length(sm_compute(data, "OMerging")$OMerging) == 1)
    expect_true(is.numeric(sm_compute(data, "F")$F))
    expect_true(length(sm_compute(data, "F")$F) == 1)

    area_df <- get_areas(sm_ref(data), sm_seg(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- dplyr::bind_rows(dplyr::as_tibble(test_y_c(area_df)),
                             dplyr::as_tibble(test_y_d(area_df))) %>%
        dplyr::distinct(seg_id, ref_id, .keep_all = TRUE)

    expect_true(test_OS2(y_prime) == sm_compute(data, "OS2"))
    expect_true(test_US2(y_prime) == sm_compute(data, "US2"))
    expect_true(test_OS1(y_star)  == sm_compute(data, "OS1"))
    expect_true(test_US1(y_star)  == sm_compute(data, "US1"))
    expect_true(test_overMerging(y_star)  == sm_compute(data, "OMerging"))
    expect_true(test_underMerging(y_star) == sm_compute(data, "UMerging"))
    expect_true(test_AFI(y_prime) == sm_compute(data, "AFI"))
    expect_true(test_QR(y_star) == sm_compute(data, "QR"))
    expect_true(test_D_index(y_star) == sm_compute(data, "D_index"))
    expect_true(test_precision(x_prime) == sm_compute(data, "precision"))
    expect_true(test_recall(y_prime) == sm_compute(data, "recall"))
    expect_true(test_M(y_prime) == sm_compute(data, "M"))
    expect_true(all(test_RAsub(y_tilde) == unlist(sm_compute(data, "RAsub"))))
    expect_true(all(test_RAsuper(y_tilde) == unlist(sm_compute(data, "RAsuper"))))
    expect_true(all(test_PI(y_tilde) == sm_compute(data, "PI")$PI))
    expect_true(all(test_OS3(y_cd) == sm_compute(data, "OS3")$OS3))
    expect_true(all(test_US3(y_cd) == sm_compute(data, "US3")$US3))
    expect_true(all(test_ED3(area_df) == sm_compute(data, "ED3")$ED3))
    expect_true(
        test_F_measure(test_precision(x_prime),
                       test_recall(y_prime)) == sm_compute(data, "F_measure")
    )

})



test_that("two segments inside works", {

    ref_sf <- sf::st_sf(geometry = sf::st_sfc(p05 * 10))
    seg_sf <- sf::st_sf(geometry = sf::st_sfc(p00 + 51,
                                              (p00 * 2) + 57))

    data <- sm_read(ref_sf, seg_sf)

    expect_true(is.numeric(sm_compute(data, "OS2")$OS2))
    expect_true(length(sm_compute(data, "OS2")$OS2) == 1)
    expect_true(is.numeric(sm_compute(data, "OS1")$OS1))
    expect_true(length(sm_compute(data, "OS1")$OS1) == 2)
    expect_true(is.numeric(sm_compute(data, "US2")$US2))
    expect_true(length(sm_compute(data, "US2")$US2) == 1)
    expect_true(is.numeric(sm_compute(data, "US1")$US1))
    expect_true(length(sm_compute(data, "US1")$US1) == 2)
    expect_true(length(sm_compute(data, "US1")$US1) == 2)
    expect_true(is.numeric(sm_compute(data, "AFI")$AFI))
    expect_true(length(sm_compute(data, "AFI")$AFI) == 1)
    expect_true(is.numeric(sm_compute(data, "QR")$QR))
    expect_true(length(sm_compute(data, "QR")$QR) == 2)
    expect_true(is.numeric(sm_compute(data, "D_index")$D_index))
    expect_true(length(sm_compute(data, "D_index")$D_index) == 2)
    expect_true(is.numeric(sm_compute(data, "D_index")$D_index))
    expect_true(length(sm_compute(data, "D_index")$D_index) == 2)
    expect_true(!is.nan(sm_compute(data, "precision")$precision))
    expect_true(!is.na(sm_compute(data, "precision")$precision))
    expect_true(is.numeric(sm_compute(data, "precision")$precision))
    expect_true(length(sm_compute(data, "precision")$precision) == 1)
    expect_true(!is.nan(sm_compute(data, "recall")$recall))
    expect_true(!is.na(sm_compute(data, "recall")$recall))
    expect_true(is.numeric(sm_compute(data, "recall")$recall))
    expect_true(length(sm_compute(data, "recall")$recall) == 1)
    expect_true(is.numeric(sm_compute(data, "M")$M))
    expect_true(length(sm_compute(data, "M")$M) == 1)
    expect_true(is.numeric(sm_compute(data, "E")$E))
    expect_true(length(sm_compute(data, "E")$E) == 2)
    expect_true(is.numeric(sm_compute(data, "RAsub")$RAsub))
    expect_true(length(sm_compute(data, "RAsub")$RAsub) == 2)
    expect_true(is.numeric(sm_compute(data, "RAsuper")$RAsuper))
    expect_true(length(sm_compute(data, "RAsuper")$RAsuper) == 2)
    expect_true(is.numeric(sm_compute(data, "PI")$PI))
    expect_true(length(sm_compute(data, "PI")$PI) == 2)
    expect_true(all(!is.na(sm_compute(data, "OS3")$OS3)))
    expect_true(all(!is.na(sm_compute(data, "US3")$US3)))
    expect_true(all(!is.na(sm_compute(data, "ED3")$ED3)))
    expect_true(!is.nan(sm_compute(data, "F_measure")$F_measure))
    expect_true(!is.na(sm_compute(data, "F_measure")$F_measure))
    expect_true(length(sm_compute(data, "F_measure")$F_measure) == 1)

    area_df <- get_areas(sm_ref(data), sm_seg(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- dplyr::bind_rows(dplyr::as_tibble(test_y_c(area_df)),
                             dplyr::as_tibble(test_y_d(area_df))) %>%
        dplyr::distinct(seg_id, ref_id, .keep_all = TRUE)

    expect_true(mean(test_OS2(y_prime)) == sm_compute(data, "OS2"))
    expect_true(mean(test_US2(y_prime)) == sm_compute(data, "US2"))
    expect_true(all(test_OS1(y_star)  == sm_compute(data, "OS1")$OS1))
    expect_true(all(test_US1(y_star)  == sm_compute(data, "US1")$US1))
    expect_true(all(test_overMerging(y_star)  == sm_compute(data, "OMerging")$OMerging))
    expect_true(all(test_underMerging(y_star) == sm_compute(data, "UMerging")$UMerging))
    expect_true(mean(test_AFI(y_prime)) == sm_compute(data, "AFI")$AFI)
    expect_true(all(test_QR(y_star) == sm_compute(data, "QR")$QR))
    expect_true(all(test_D_index(y_star) == sm_compute(data, "D_index")$D_index))
    expect_true(test_precision(x_prime) == sm_compute(data, "precision")$precision)
    expect_true(test_recall(y_prime) == sm_compute(data, "recall"))
    expect_true(mean(test_M(y_prime)) == sm_compute(data, "M")$M)
    expect_true(all(test_RAsub(y_tilde) == unlist(sm_compute(data, "RAsub"))))
    expect_true(all(test_RAsuper(y_tilde) == unlist(sm_compute(data, "RAsuper"))))
    expect_true(all(test_PI(y_tilde) == sm_compute(data, "PI")$PI))
    if (nrow(y_cd) == 0) {
        expect_true(is.na(sm_compute(data, "OS3")))
        expect_true(is.na(sm_compute(data, "US3")))
        expect_true(is.na(sm_compute(data, "ED3")))
    }
    expect_true(
        test_F_measure(test_precision(x_prime),
                       test_recall(y_prime)) ==
            sm_compute(data, "F_measure")$F_measure)
})



test_that("grid works", {

    ref <- sf::st_sf(geometry = sf::st_sfc(p05))
    seg <- sf::st_sf(geometry = sf::st_sfc(p05 + c(-0.5, -0.5),
                                           p05 + c( 0.5, -0.5),
                                           p05 + c( 0.5,  0.5),
                                           p05 + c(-0.5,  0.5)))
    data <- sm_read(ref, seg)

    expect_true(is.numeric(sm_compute(data, "OS2")$OS2))
    expect_true(length(sm_compute(data, "OS2")$OS2) == 4)
    expect_true(is.numeric(sm_compute(data, "OS1")$OS1))
    expect_true(length(sm_compute(data, "OS1")$OS1) == 4)
    expect_true(is.numeric(sm_compute(data, "US2")$US2))
    expect_true(length(sm_compute(data, "US2")$US2) == 4)
    expect_true(is.numeric(sm_compute(data, "US1")$US1))
    expect_true(length(sm_compute(data, "US1")$US1) == 4)
    expect_true(length(sm_compute(data, "US1")$US1) == 4)
    expect_true(is.numeric(sm_compute(data, "AFI")$AFI))
    expect_true(length(sm_compute(data, "AFI")$AFI) == 4)
    expect_true(is.numeric(sm_compute(data, "QR")$QR))
    expect_true(length(sm_compute(data, "QR")$QR) == 4)
    expect_true(is.numeric(sm_compute(data, "D_index")$D_index))
    expect_true(length(sm_compute(data, "D_index")$D_index) == 4)
    expect_true(is.numeric(sm_compute(data, "D_index")$D_index))
    expect_true(length(sm_compute(data, "D_index")$D_index) == 4)
    expect_true(!is.nan(sm_compute(data, "precision")$precision))
    expect_true(!is.na(sm_compute(data, "precision")$precision))
    expect_true(is.numeric(sm_compute(data, "precision")$precision))
    expect_true(length(sm_compute(data, "precision")$precision) == 1)
    expect_true(!is.nan(sm_compute(data, "recall")$recall))
    expect_true(!is.na(sm_compute(data, "recall")$recall))
    expect_true(is.numeric(sm_compute(data, "recall")$recall))
    expect_true(length(sm_compute(data, "recall")$recall) == 1)
    expect_true(is.numeric(sm_compute(data, "M")$M))
    expect_true(length(sm_compute(data, "M")$M) == 4)
    expect_true(is.numeric(sm_compute(data, "E")$E))
    expect_true(length(sm_compute(data, "E")$E) == 4)
    expect_true(is.numeric(sm_compute(data, "RAsub")$RAsub))
    expect_true(length(sm_compute(data, "RAsub")$RAsub) == 4)
    expect_true(is.numeric(sm_compute(data, "RAsuper")$RAsuper))
    expect_true(length(sm_compute(data, "RAsuper")$RAsuper) == 4)
    expect_true(is.numeric(sm_compute(data, "PI")$PI))
    expect_true(length(sm_compute(data, "PI")$PI) == 4)
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_true(!is.nan(sm_compute(data, "F_measure")$F_measure))
    expect_true(!is.na(sm_compute(data, "F_measure")$F_measure))
    expect_true(length(sm_compute(data, "F_measure")$F_measure) == 1)

    area_df <- get_areas(sm_ref(data), sm_seg(data))
    x_prime <- test_x_prime(area_df)
    y_prime <- test_y_prime(area_df)
    y_star  <- test_y_star(area_df)
    y_tilde <- test_y_tilde(area_df)
    y_cd <- dplyr::bind_rows(dplyr::as_tibble(test_y_c(area_df)),
                             dplyr::as_tibble(test_y_d(area_df))) %>%
        dplyr::distinct(seg_id, ref_id, .keep_all = TRUE)

    expect_true(mean(test_OS2(y_prime)) == summary(sm_compute(data, "OS2")))
    expect_true(mean(test_US2(y_prime)) == summary(sm_compute(data, "US2")))
    expect_true(all(test_OS1(y_star)  == sm_compute(data, "OS1")$OS1))
    expect_true(all(test_US1(y_star)  == sm_compute(data, "US1")$US1))
    expect_true(all(test_overMerging(y_star)  == sm_compute(data, "OMerging")$OMerging))
    expect_true(all(test_underMerging(y_star) == sm_compute(data, "UMerging")$UMerging))
    expect_true(mean(test_AFI(y_prime)) == summary(sm_compute(data, "AFI")))
    expect_true(all(test_QR(y_star) == sm_compute(data, "QR")$QR))
    expect_true(all(test_D_index(y_star) == sm_compute(data, "D_index")$D_index))
    expect_true(test_precision(x_prime) == sm_compute(data, "precision"))
    expect_true(test_recall(y_prime) == sm_compute(data, "recall"))
    expect_true(mean(test_M(y_prime)) == summary(sm_compute(data, "M")))
    expect_true(all(test_RAsub(y_tilde) == unlist(sm_compute(data, "RAsub"))))
    expect_true(all(test_RAsuper(y_tilde) == unlist(sm_compute(data, "RAsuper"))))
    expect_true(all(test_PI(y_tilde) == sm_compute(data, "PI")$PI))
    expect_true(sm_is_empty(sm_compute(data, "OS3")))
    expect_true(sm_is_empty(sm_compute(data, "US3")))
    expect_true(sm_is_empty(sm_compute(data, "ED3")))
    expect_true(
        test_F_measure(test_precision(x_prime),
                       test_recall(y_prime)) == sm_compute(data, "F_measure"))

})
