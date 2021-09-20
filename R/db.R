rows_inset <- function(x, y) {

    stopifnot(inherits(x, "sf"))
    stopifnot(inherits(y, "sf"))
    stopifnot(c("ref_id", "seg_id") %in% names(x))
    stopifnot(c("ref_id", "seg_id") %in% names(y))


    which(paste0(x$seg_id, ";", x$ref_id) %in%
              paste0(y$seg_id, ";", y$ref_id))
}

rows_distinct <- function(x) {

    stopifnot(inherits(x, "sf"))
    stopifnot(c("ref_id", "seg_id") %in% names(x))

    id <- paste0(x$seg_id, ";", x$ref_id)
    match(unique(id), id)
}

area <- function(x, order = NULL) {

    stopifnot(inherits(x, "sf"))

    res <- suppressWarnings(suppressMessages(
            sf::st_area(x)
    ))

    if (inherits(res, "units"))
        res <- units::drop_units(res)

    if (!is.null(order))
        return(res[order])
    return(res)
}

centroid <- function(x) {

    stopifnot(inherits(x, "sf"))

    suppressWarnings(suppressMessages(
        sf::st_centroid(x, of_largest_polygon = FALSE)
    ))
}

intersection <- function(x, y, touches = TRUE) {

    stopifnot(inherits(x, "sf"))
    stopifnot(inherits(y, "sf"))

    res <- suppressWarnings(suppressMessages({
        sf::st_intersection(x = x, y = y)
    }))

    # filter only polygons
    if (!touches)
        res <- res[area(res) > 0,]

    # # post condition
    # stopifnot(nrow(res) > 0)

    res
}

union2 <- function(x, seg_sf, ref_sf) {

    stopifnot(inherits(x, "sf"))
    stopifnot(c("ref_id", "seg_id") %in% names(x))

    dplyr::bind_rows(lapply(seq_len(nrow(x)), function(i) {
        suppressWarnings(suppressMessages({
            sf::st_union(x = seg_sf[ref_id(x[i,]),],
                         y = ref_sf[seg_id(x[i,]),])
        }))
    }))
}

ref_id <- function(x) {
    stopifnot(inherits(x, "sf"))
    stopifnot(c("ref_id") %in% names(x))

    x$ref_id
}

bind_all <- function(...) {

    dots <- list(...)
    stopifnot(all(vapply(dots, inherits, logical(1), "sf")))

    res <- suppressWarnings(
        do.call(rbind, args = dots)
    )
    suppressWarnings(
        res[rows_distinct(res),]
    )
}

seg_id <- function(x) {

    stopifnot(inherits(x, "sf"))
    stopifnot(c("seg_id") %in% names(x))

    x[["seg_id"]]
}

.db_m <- structure(
    list(
        ### metrics
        "OS2" = list(
            depends    = c("Y_prime"),
            expression = quote({
                1 - area(Y_prime) / area(ref_sf, order = ref_id(Y_prime))
            }),
            citation   = "Persello and Bruzzone (2010)"
        ),
        "OS1" = list(
            depends    = c("Y_star"),
            expression = quote({
                1 - area(Y_star) / area(ref_sf, order = ref_id(Y_star))
            }),
            citation   = "Clinton et al. (2010)"
        ),
        "US2" = list(
            depends    = c("Y_prime"),
            expression = quote({
                1 - area(Y_prime) / area(seg_sf, order = seg_id(Y_prime))
            }),
            citation   = "Persello and Bruzzone (2010)"
        ),
        "US1" = list(
            depends    = c("Y_star"),
            expression = quote({
                1 - area(Y_star) / area(seg_sf, order = seg_id(Y_star))
            }),
            citation   = "Clinton et al. (2010)"
        ),
        "AFI" = list(
            depends    = c("Y_prime"),
            expression = quote({
                (area(ref_sf, order = ref_id(Y_prime)) -
                     area(seg_sf, order = seg_id(Y_prime))) /
                    area(ref_sf, order = ref_id(Y_prime))

            }),
            citation   = "Lucieer and Stein (2002) and Clinton et al. (2010)"
        ),
        "QR" = list(
            depends    = c("Y_star"),
            expression = quote({
                1 - area(Y_star) / area(union2(Y_star, seg_sf, ref_sf))
            }),
            citation   = "Weidner (2008) and Clinton et al. (2010)"
        ),
        "D_index" = list(
            depends    = c("Y_star"),
            expression = quote({
                sqrt((
                    (1 - area(Y_star) /
                         area(ref_sf, order = ref_id(Y_star))) ^ 2 +
                        (1 - area(Y_star) /
                             area(seg_sf, order = seg_id(Y_star))) ^ 2) / 2)
            }),
            citation   = "Levine and Nazif (1982) and Clinton et al. (2010)"
        ),
        "precision" = list(
            depends    = c("X_prime"),
            expression = quote({
                sum(area(X_prime)) / sum(area(seg_sf, order = seg_id(X_prime)))
            }),
            citation   = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        ),
        "recall" = list(
            depends    = c("Y_prime"),
            expression = quote({
                sum(area(Y_prime)) / sum(area(ref_sf, order = ref_id(Y_prime)))
            }),
            citation   = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        ),
        "underMerging" = list(
            depends    = c("Y_star"),
            expression = quote({
                (area(ref_sf, order = ref_id(Y_star)) - area(Y_star)) /
                    area(ref_sf, order = ref_id(Y_star))
            }),
            citation   = "Levine and Nazif (1982) and Clinton et al. (2010)"
        ),
        "overMerging" = list(
            depends    = c("Y_star"),
            expression = quote({
                (area(seg_sf, order = seg_id(Y_star)) - area(Y_star)) /
                    area(ref_sf, order = ref_id(Y_star))
            }),
            citation   = "Levine and Nazif (1982) and Clinton et al. (2010)"
        ),
        "M" = list(
            depends    = c("Y_prime"),
            expression = quote({
                sqrt(area(Y_prime) ^ 2 /
                         area(ref_sf, order = ref_id(Y_prime)) /
                         area(seg_sf, order = seg_id(Y_prime)))
            }),
            citation   = "Janssen and Molenaar (1995) and Feitosa et al. (2010)"
        ),
        # TODO: check formula in Carleer et al. (2005)
        "E" = list(
            depends    = c("X_prime"),
            expression = quote({
                (area(seg_sf, order = seg_id(X_prime)) - area(X_prime)) /
                    area(seg_sf, order = seg_id(X_prime)) * 100
            }),
            citation   = "Carleer et al. (2005)"
        ),
        "RAsub" = list(
            depends    = c("Y_tilde"),
            expression = quote({
                area(Y_tilde) / area(ref_sf, order = ref_id(Y_tilde))
            }),
            citation   = "Möller et al. (2007) and Clinton et al. (2010)"
        ),
        "RAsuper" = list(
            depends    = c("Y_tilde"),
            expression = quote({
                area(Y_tilde) / area(seg_sf, order = seg_id(Y_tilde))
            }),
            citation   = "Möller et al. (2007) and Clinton et al. (2010)"
        ),
        "PI" = list(
            depends    = c("Y_tilde"),
            expression = quote({
                area(Y_tilde) ^ 2 /
                    area(ref_sf, order = ref_id(Y_tilde)) /
                    area(ref_sf, order = ref_id(Y_tilde))
            }),
            citation   = "van Coillie et al. (2008)"
        ),
        "F" = list(
            depends    = c("X_prime"),
            expression = quote({
                (area(seg_sf, order = seg_id(X_prime)) +
                     area(ref_sf, order = ref_id(X_prime)) -
                     2 * area(X_prime)) /
                    area(seg_sf, order = seg_id(X_prime))
            }),
            citation   = "Costa et al. (2008)"
        ),
        "OS3" = list(
            depends    = c("Y_cd"),
            expression = quote({
                1 - area(Y_cd) / area(ref_sf, order = ref_id(Y_cd))
            }),
            citation   = "Yang et al. (2014)"
        ),
        "US3" = list(
            depends    = c("Y_cd"),
            expression = quote({
                1 - area(Y_cd) / area(seg_sf, order = seg_id(Y_cd))
            }),
            citation   = "Yang et al. (2014)"
        ),
        "ED3" = list(
            depends    = c("Y_cd"),
            expression = quote({
                sqrt(((1 - area(Y_cd) /
                           area(ref_sf, order = ref_id(Y_cd)))^2 +
                          (1 - area(Y_cd) /
                               area(seg_sf, order = seg_id(Y_cd)))^2) / 2)
            }),
            citation   = "Yang et al. (2014)"
        ),
        "F_measure" = list(
            depends    = c("X_prime", "Y_prime"),
            expression = quote({
                1 / ((0.5 / (sum(area(X_prime)) /
                                 sum(area(seg_sf, order = seg_id(X_prime)))))
                     + ((1 - 0.5) / (sum(area(Y_prime)) /
                                         sum(area(ref_sf, order = ref_id(Y_prime))))))
            }),
            citation   = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        )
    )
)

.db_f <- structure(
    list(
        "ref_sf"  = list(
            depends    = character(),
            expression = quote({})
        ),
        "seg_sf"  = list(
            depends    = character(),
            expression = quote({})
        ),
        "Y_tilde" = list(
            depends    = character(),
            expression = quote({
                intersection(x = ref_sf, y = seg_sf, touches = FALSE)
            })
        ),
        "X_tilde" = list(
            depends    = character(),
            expression = quote({
                intersection(x = seg_sf, y = ref_sf, touches = FALSE)
            })
        ),
        "Y_prime" = list(
            depends    = c("Y_tilde"),
            expression = quote({
                suppressWarnings(
                    Y_tilde %>%
                        dplyr::mutate(inter_area = area(.)) %>%
                        dplyr::group_by(ref_id) %>%
                        dplyr::filter(inter_area == max(inter_area)) %>%
                        dplyr::select(-inter_area) %>%
                        dplyr::slice(1) %>%
                        dplyr::ungroup()
                )
            })
        ),
        "X_prime" = list(
            depends    = c("X_tilde"),
            expression = quote({
                suppressWarnings(
                    X_tilde %>%
                        dplyr::mutate(inter_area = area(.)) %>%
                        dplyr::group_by(seg_id) %>%
                        dplyr::filter(inter_area == max(inter_area)) %>%
                        dplyr::select(-inter_area) %>%
                        dplyr::slice(1) %>%
                        dplyr::ungroup()
                )
            })
        ),
        "ref_centroids" = list(
            depends    = character(),
            expression = quote({
                centroid(ref_sf)
            })
        ),
        "Y_a" = list(
            depends    = c("Y_tilde", "ref_centroids"),
            expression = quote({
                Y_a <- intersection(x = ref_centroids, y = seg_sf)

                suppressWarnings(
                    Y_tilde[rows_inset(Y_tilde, Y_a),]
                )
            })
        ),
        "seg_centroids" = list(
            depends    = character(),
            expression = quote({
                centroid(seg_sf)
            })
        ),
        "Y_b" = list(
            depends    = c("Y_tilde", "seg_centroids"),
            expression = quote({
                Y_b <- intersection(x = seg_centroids, y = ref_sf)
                suppressWarnings(
                    Y_tilde[rows_inset(Y_tilde, Y_b),]
                )
            })
        ),
        "Y_c" = list(
            depends    = c("Y_tilde"),
            expression = quote({

                seg_area <- area(seg_sf, order = seg_id(Y_tilde))
                inter_area <- area(Y_tilde)
                suppressWarnings(
                    Y_tilde[inter_area / seg_area > 0.5,]
                )
            })
        ),
        "Y_d" = list(
            depends    = c("Y_tilde"),
            expression = quote({

                ref_area <- area(ref_sf, order = ref_id(Y_tilde))
                inter_area <- area(Y_tilde)
                suppressWarnings(
                    Y_tilde[inter_area / ref_area > 0.5,]
                )
            })
        ),
        "Y_star" = list(
            depends    = c("Y_a", "Y_b", "Y_c", "Y_d"),
            expression = quote({

                bind_all(Y_a, Y_b, Y_c, Y_d)
            })
        ),
        "Y_cd" = list(
            depends    = c("Y_c", "Y_d"),
            expression = quote({

                bind_all(Y_c, Y_d)
            })
        ),
        "Y_e" = list(
            depends    = c("Y_tilde"),
            expression = quote({

                seg_area <- area(seg_sf, order = seg_id(Y_tilde))
                inter_area <- area(Y_tilde)

                suppressWarnings(
                    Y_tilde[inter_area / seg_area == 1,]
                )
            })
        ),
        "Y_f" = list(
            depends    = c("Y_tilde"),
            expression = quote({

                seg_area <- area(seg_sf, order = seg_id(Y_tilde))
                inter_area <- area(Y_tilde)

                suppressWarnings(
                    Y_tilde[inter_area / seg_area > 0.55,]
                )
            })
        ),
        "Y_g" = list(
            depends    = c("Y_tilde"),
            expression = quote({

                seg_area <- area(seg_sf, order = seg_id(Y_tilde))
                inter_area <- area(Y_tilde)

                suppressWarnings(
                    Y_tilde[inter_area / seg_area > 0.75,]
                )
            })
        )),
    class = "db_metric"
)



db_summary <- list(
    "mean"   = list(
        expression = quote(mean(metric, na.rm = TRUE))
    ),
    "w_mean" = list(
        expression = quote(weighted.mean(metric, weight, na.rm = TRUE))
    ),
    "identity" = list(
        expression = quote(metric)
    )
)

.db_fields <- function(d, where = NULL, ...) {

    if (is.null(where))
        return(names(d))

    names(d[vapply(d, where, logical(1), ...)])
}

.db_get <- function(d, key) {

    stopifnot(length(key) == 1)
    stopifnot(key %in% names(d))
    d[[key]]
}

.metric_check <- function(m, len = NULL) {

    stopifnot(inherits(m, "metric"))
    stopifnot(length(m) <= 1)
    stopifnot(all(c("ref_sf", "seg_sf") %in% .metric_fields(m)))
    if (length(m) == 1) {
        stopifnot(!is.null(names(m)))
        stopifnot(names(m) %in% .db_fields(d = .db_m))
    }
    stopifnot(all(.metric_fields(m) %in% .db_fields(d = .db_f)))
    if (!is.null(len))
        stopifnot(length(m) == len)
}

.metric_env <- function(m) {
    attr(m, which = ".env", exact = TRUE)
}

.metric_eval <- function(m, expr, parameters = list()) {
    eval(expr, envir = parameters, enclos = .metric_env(m))
}

.metric_fields <- function(m) {
    ls(.metric_env(m))
}

.metric_exists <- function(m, field) {
    exists(field, envir = .metric_env(m))
}

.metric_set <- function(m, field, value) {
    assign(field, value, envir = .metric_env(m))
    m
}

.metric_get <- function(m, field) {
    get(field, envir = .metric_env(m), inherits = FALSE)
}

.metric_clear <- function(m) {
    fields <- .metric_fields(m)
    fields <- fields[!fields %in% c("ref_sf", "seg_sf")]
    rm(list = fields, envir = .metric_env(m), inherits = FALSE)
    invisible(NULL)
}

.metric_compute_field <- function(m, field) {

    f <- .db_get(d = .db_f, key = field)
    stopifnot(all(f[["depends"]] %in% .db_fields(d = .db_f)))

    for (dep in f[["depends"]]) {
        if (dep %in% .metric_fields(m)) next
        .metric_compute_field(m = m, field = dep)
    }
    value <- .metric_eval(m = m, expr = f[["expression"]])
    .metric_set(m, field = field, value = value)
}

.metric_compute <- function(m, metric, parameters = list()) {

    f <- .db_get(d = .db_m, key = metric)
    stopifnot(all(f[["depends"]] %in% .db_fields(d = .db_f)))

    for (dep in f[["depends"]]) {
        if (dep %in% .metric_fields(m)) next
        .metric_compute_field(m = m, field = dep)
    }

    m[[1]] <- .metric_eval(m = m, expr = f[["expression"]],
                           parameters = parameters)
    names(m) <- metric
    m
}

#' @export
metric <- function(ref_sf, seg_sf) {

    if (is.character(ref_sf))
        ref_sf <- sf::read_sf(ref_sf)
    stopifnot(inherits(ref_sf, "sf"))

    if (is.character(seg_sf))
        seg_sf <- sf::read_sf(seg_sf)
    stopifnot(inherits(seg_sf, "sf"))

    stopifnot(sf::st_crs(ref_sf) == sf::st_crs(seg_sf))

    ref_sf[["ref_id"]] <- seq_len(nrow(ref_sf))
    seg_sf[["seg_id"]] <- seq_len(nrow(seg_sf))

    .env <- environment()

    structure(list(),
              .env = .env,
              class = c("metric"))
}

#' @export
list_metrics <- function() {

    .db_fields(d = .db_m)
}

#' @export
desc_metric <- function(metric) {

    stopifnot(metric %in% list_metrics())

    f <- .db_get(d = .db_m, key = metric)
    cat(paste("-", metric), fill = TRUE)
    # cat(paste(f[["description"]]), fill = TRUE)
    cat(paste("citation:", f[["citation"]]), fill = TRUE)
}


#' @export
get_metric <- function(m, metric, ...) {

    .metric_check(m = m)
    stopifnot(metric %in% list_metrics())

    .metric_compute(m = m, metric = metric, parameters = list(...))
}

#' @export
get_ref_area <- function(m) {

    .metric_check(m = m, len = 1)

    f <- .db_get(d = .db_m, key = names(m))
    ordering <- f[["depends"]][[1]]

    stopifnot(ordering %in% .db_fields(d = .db_f))

    ref_sf <- .metric_get(m = m, field = "ref_sf")
    ref_rows <- ref_id(.metric_get(m = m, field = ordering))

    area(ref_sf, order = ref_rows)
}

#' @export
get_seg_area <- function(m) {
    .metric_check(m = m, len = 1)

    f <- .db_get(d = .db_m, key = names(m))
    ordering <- f[["depends"]][[1]]

    stopifnot(ordering %in% .db_fields(d = .db_f))

    seg_sf <- .metric_get(m = m, field = "seg_sf")
    seg_rows <- seg_id(.metric_get(m = m, field = ordering))

    area(seg_sf, order = seg_rows)
}

#' @export
get_inter_area <- function(m) {
    .metric_check(m = m, len = 1)

    f <- .db_get(d = .db_m, key = names(m))
    field <- f[["depends"]][[1]]

    stopifnot(field %in% .db_fields(d = .db_f))

    area(.metric_get(m = m, field = field))
}

#' @exportS3Method
plot.metric <- function(m, ...) {

    # TODO: Don't fill the polygons, just change borders' colors

    ref_sf <- dplyr::transmute(ref_sf(m), type = "reference")
    seg_sf <- dplyr::transmute(seg_sf(m), type = "segmentation")
    data_sf <- rbind(ref_sf, seg_sf)
    plot(data_sf,
         col = c("blue", "red"),
         main = "Reference (blue) and Segmentation (red)"
    )
}

#' @exportS3Method
summary.metric <- function(m, w = NULL, ...) {

    stopifnot(inherits(m, "metric"))

    if (!is.null(w))
        return(lapply(m, weighted.mean, w = w))

    lapply(m, mean)
}

