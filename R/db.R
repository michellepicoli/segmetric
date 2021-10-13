
.db_m <- structure(
    list(
        ### metrics
        "OS2" = list(
            depends    = c("Y_prime"),
            fn         = OS2,
            citation   = "Persello and Bruzzone (2010)"
        ),
        "OS1" = list(
            depends    = c("Y_star"),
            fn         = OS1,
            citation   = "Clinton et al. (2010)"
        ),
        "US2" = list(
            depends    = c("Y_prime"),
            fn         = US2,
            citation   = "Persello and Bruzzone (2010)"
        ),
        "US1" = list(
            depends    = c("Y_star"),
            fn         = US1,
            citation   = "Clinton et al. (2010)"
        ),
        "AFI" = list(
            depends    = c("Y_prime"),
            fn         = AFI,
            citation   = "Lucieer and Stein (2002) and Clinton et al. (2010)"
        ),
        "QR" = list(
            depends    = c("Y_star"),
            fn         = QR,
            citation   = "Weidner (2008) and Clinton et al. (2010)"
        ),
        "D_index" = list(
            depends    = c("Y_star"),
            fn         = D_index,
            citation   = "Levine and Nazif (1982) and Clinton et al. (2010)"
        ),
        "precision" = list(
            depends    = c("X_prime"),
            fn         = precision,
            citation   = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        ),
        "recall" = list(
            depends    = c("Y_prime"),
            fn         = recall,
            citation   = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        ),
        "underMerging" = list(
            depends    = c("Y_star"),
            fn         = UMerging,
            citation   = "Levine and Nazif (1982) and Clinton et al. (2010)"
        ),
        "overMerging" = list(
            depends    = c("Y_star"),
            fn         = OMerging,
            citation   = "Levine and Nazif (1982) and Clinton et al. (2010)"
        ),
        "M" = list(
            depends    = c("Y_prime"),
            fn         = M,
            citation   = "Janssen and Molenaar (1995) and Feitosa et al. (2010)"
        ),
        # TODO: check formula in Carleer et al. (2005)
        "E" = list(
            depends    = c("X_prime"),
            fn         = E,
            citation   = "Carleer et al. (2005)"
        ),
        "RAsub" = list(
            depends    = c("Y_tilde"),
            fn         = RAsub,
            citation   = "Möller et al. (2007) and Clinton et al. (2010)"
        ),
        "RAsuper" = list(
            depends    = c("Y_tilde"),
            fn         = RAsuper,
            citation   = "Möller et al. (2007) and Clinton et al. (2010)"
        ),
        "PI" = list(
            depends    = c("Y_tilde"),
            fn         = PI,
            citation   = "van Coillie et al. (2008)"
        ),
        "F" = list(
            depends    = c("X_prime"),
            fn         = F1,
            citation   = "Costa et al. (2008)"
        ),
        "OS3" = list(
            depends    = c("Y_cd"),
            fn         = OS3,
            citation   = "Yang et al. (2014)"
        ),
        "US3" = list(
            depends    = c("Y_cd"),
            fn         = US3,
            citation   = "Yang et al. (2014)"
        ),
        "ED3" = list(
            depends    = c("Y_cd"),
            fn         = ED3,
            citation   = "Yang et al. (2014)"
        ),
        "F_measure" = list(
            depends    = c("X_prime", "Y_prime"),
            fn         = F_measure,
            citation   = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        )
    )
)

# 
# .db_f <- structure(
#     list(
#         "ref_sf"  = list(
#             depends    = character(),
#             expression = quote({})
#         ),
#         "seg_sf"  = list(
#             depends    = character(),
#             expression = quote({})
#         ),
#         "Y_tilde" = list(
#             depends    = character(),
#             expression = quote({
#                 intersection(x = ref_sf, y = seg_sf, touches = FALSE)
#             })
#         ),
#         "X_tilde" = list(
#             depends    = character(),
#             expression = quote({
#                 intersection(x = seg_sf, y = ref_sf, touches = FALSE)
#             })
#         ),
#         "Y_prime" = list(
#             depends    = c("Y_tilde"),
#             expression = quote({
#                 suppressWarnings(
#                     Y_tilde %>% 
#                         dplyr::mutate(inter_area = area(.)) %>%
#                         dplyr::group_by(ref_id) %>% 
#                         dplyr::filter(inter_area == max(inter_area)) %>%
#                         dplyr::select(-inter_area) %>% 
#                         dplyr::slice(1) %>% 
#                         dplyr::ungroup()
#                 )
#             })
#         ),
#         "X_prime" = list(
#             depends    = c("X_tilde"),
#             expression = quote({
#                 suppressWarnings(
#                     X_tilde %>% 
#                         dplyr::mutate(inter_area = area(.)) %>%
#                         dplyr::group_by(seg_id) %>% 
#                         dplyr::filter(inter_area == max(inter_area)) %>%
#                         dplyr::select(-inter_area) %>% 
#                         dplyr::slice(1) %>% 
#                         dplyr::ungroup()
#                 )
#             })
#         ),
#         "ref_centroids" = list(
#             depends    = character(),
#             expression = quote({
#                 centroid(ref_sf)
#             })
#         ),
#         "Y_a" = list(
#             depends    = c("Y_tilde", "ref_centroids"),
#             expression = quote({
#                 Y_a <- intersection(x = ref_centroids, y = seg_sf)
#                 
#                 suppressWarnings(
#                     Y_tilde[rows_inset(Y_tilde, Y_a),]
#                 )
#             })
#         ),
#         "seg_centroids" = list(
#             depends    = character(),
#             expression = quote({
#                 centroid(seg_sf)
#             })
#         ),
#         "Y_b" = list(
#             depends    = c("Y_tilde", "seg_centroids"),
#             expression = quote({
#                 Y_b <- intersection(x = seg_centroids, y = ref_sf)
#                 suppressWarnings(
#                     Y_tilde[rows_inset(Y_tilde, Y_b),]
#                 )
#             })
#         ),
#         "Y_c" = list(
#             depends    = c("Y_tilde"),
#             expression = quote({
#                 
#                 seg_area <- area(seg_sf, order = seg_id(Y_tilde))
#                 inter_area <- area(Y_tilde)
#                 suppressWarnings(
#                     Y_tilde[inter_area / seg_area > 0.5,]
#                 )
#             })
#         ),
#         "Y_d" = list(
#             depends    = c("Y_tilde"),
#             expression = quote({
#                 
#                 ref_area <- area(ref_sf, order = ref_id(Y_tilde))
#                 inter_area <- area(Y_tilde)
#                 suppressWarnings(
#                     Y_tilde[inter_area / ref_area > 0.5,]
#                 )
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
#                 suppressWarnings(
#                     Y_tilde[inter_area / seg_area == 1,]
#                 )
#             })
#         ),
#         "Y_f" = list(
#             depends    = c("Y_tilde"),
#             expression = quote({
#                 
#                 seg_area <- area(seg_sf, order = seg_id(Y_tilde))
#                 inter_area <- area(Y_tilde)
#                 
#                 suppressWarnings(
#                     Y_tilde[inter_area / seg_area > 0.55,]
#                 )
#             })
#         ),
#         "Y_g" = list(
#             depends    = c("Y_tilde"),
#             expression = quote({
#                 
#                 seg_area <- area(seg_sf, order = seg_id(Y_tilde))
#                 inter_area <- area(Y_tilde)
#                 
#                 suppressWarnings(
#                     Y_tilde[inter_area / seg_area > 0.75,]
#                 )
#             })
#         )),
#     class = "db_metric"
# )

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
    if (!is.null(len))
        stopifnot(length(m) == len)
}

.metric_env <- function(m) {
    attr(m, which = ".env", exact = TRUE)
}

.metric_eval <- function(m, fn, parameters = list()) {
    do.call(fn, args = c(list(m = m), parameters))
}

.metric_fields <- function(m) {
    ls(.metric_env(m))
}

.metric_exists <- function(m, field) {
    exists(field, envir = .metric_env(m), inherits = FALSE)
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

# .metric_compute_field <- function(m, field) {
#     
#     f <- .db_get(d = .db_f, key = field)
#     stopifnot(all(f[["depends"]] %in% .db_fields(d = .db_f)))
#     
#     for (dep in f[["depends"]]) {
#         if (dep %in% .metric_fields(m)) next
#         .metric_compute_field(m = m, field = dep)
#     }
#     value <- .metric_eval(m = m, expr = f[["expression"]])
#     .metric_set(m, field = field, value = value)
# }
# 
.metric_compute <- function(m, metric, parameters = list()) {
    
    m[[1]] <- .metric_eval(m = m, fn = metric, 
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

    class(ref_sf) <- c("ref_sf", class(ref_sf))
    class(seg_sf) <- c("seg_sf", class(seg_sf))
    
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

    # stopifnot(ordering %in% .db_fields(d = .db_f))
    
    ref_sf <- .metric_get(m = m, field = "ref_sf")
    ref_rows <- ref_id(.metric_get(m = m, field = ordering))

    area(ref_sf, order = ref_rows)
}

#' @export
get_seg_area <- function(m) {
    .metric_check(m = m, len = 1)

    f <- .db_get(d = .db_m, key = names(m))
    ordering <- f[["depends"]][[1]]

    # stopifnot(ordering %in% .db_fields(d = .db_f))
    
    seg_sf <- .metric_get(m = m, field = "seg_sf")
    seg_rows <- seg_id(.metric_get(m = m, field = ordering))

    area(seg_sf, order = seg_rows)
}

#' @export
get_inter_area <- function(m) {
    .metric_check(m = m, len = 1)

    f <- .db_get(d = .db_m, key = names(m))
    field <- f[["depends"]][[1]]

    # stopifnot(field %in% .db_fields(d = .db_f))
    
    area(.metric_get(m = m, field = field))
}

#' @exportS3Method
plot.metric <- function(m, ...) {

    ref_sf <- dplyr::transmute(ref_sf(m), type = "reference")
    seg_sf <- dplyr::transmute(seg_sf(m), type = "segmentation")

    plot(sf::st_geometry(ref_sf),
         border = 'blue',
         extent = rbind(ref_sf, seg_sf),
         main = "Reference (blue) versus Segmentation (red)")

    plot(sf::st_geometry(seg_sf),
         border = 'red',
         add = TRUE)
}

#' @exportS3Method
summary.metric <- function(m, w = NULL, ...) {

    stopifnot(inherits(m, "metric"))

    if (!is.null(w))
        return(lapply(m, weighted.mean, w = w))

    lapply(m, mean)
}

