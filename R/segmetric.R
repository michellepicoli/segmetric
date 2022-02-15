#' @title General functions
#'
#' @name segmetric_functions
#'
#' @description
#' These functions manipulate `segmetric` objects.
#' * `sm_read()`: Load the reference and segmentation polygons into segmetric.
#' * `sm_clear()`: Remove the already calculated metrics from segmetric.
#' * `print()`: Print a segmetric object.
#' * `plot()`: Plot the reference and segmentation polygons.
#' * `summary()`: Compute a measure of central tendency over the values of a metric.
#' * `sm_is_empty()`: Check if a `segmetric` object is empty.
#'
#' @param m A `segmetric` object.
#' @param object A `segmetric` object.
#' @param ref_sf A `sf` object. The reference polygons.
#' @param seg_sf A `sf` object. The segmentation polygons.
#' @param ...    Additional parameters (Not implemented).
#'
#' @returns
#' * `sm_read()`, `sm_clear()`: Return a `segmetric` object containing an
#' empty list and an environment attribute to store the necessary datasets.
#' * `sm_is_empty()`: Return a `logical` vector indicating if each computed
#' metric is empty.
#'
#' @seealso `sm_compute()`
#'
#' @examples
#' # load sample datasets
#' data("sample_ref_sf", package = "segmetric")
#' data("sample_seg_sf", package = "segmetric")
#'
#' # create segmetric object
#' m <- sm_read(ref_sf = sample_ref_sf, seg_sf = sample_seg_sf)
#'
#' # plot geometries
#' plot(m)
#'
#' # compute a metric
#' sm_compute(m, "AFI")
#'
#' # summarize the metric using mean
#' sm_compute(m, "AFI") %>% summary()
#'
#' # clear computed subsets
#' sm_clear(m)
#'
NULL


#' @rdname segmetric_functions
#' @keywords internal
.segmetric_check <- function(m) {

    stopifnot(inherits(m, "segmetric"))
    stopifnot(all(c("ref_sf", "seg_sf") %in% ls(.segmetric_env(m))))
    if (length(m) > 1) {
        stopifnot(!is.null(names(m)))
        stopifnot(names(m) %in% .db_list())
    }
}

#' @rdname segmetric_functions
#' @keywords internal
.segmetric_env <- function(m) {

    attr(m, which = ".env", exact = TRUE)
}

#' @export
#' @rdname segmetric_functions
sm_read <- function(ref_sf, seg_sf) {

    if (is.character(ref_sf))
        ref_sf <- sf::read_sf(ref_sf)
    stopifnot(inherits(ref_sf, "sf"))

    if (any(!sf::st_is_valid(ref_sf))) {

        stop(paste(
            "reference dataset has invalid geometries.",
            "Please, fix the dataset using sf::st_make_valid().",
            sep = "\n"
        ), call. = FALSE)
    }

    if (is.character(seg_sf))
        seg_sf <- sf::read_sf(seg_sf)
    stopifnot(inherits(seg_sf, "sf"))

    if (any(!sf::st_is_valid(seg_sf))) {

        stop(paste(
            "segmentation dataset has invalid geometries.",
            "Please, fix the dataset using sf::st_make_valid().",
            sep = "\n"
        ), call. = FALSE)
    }

    stopifnot(sf::st_crs(ref_sf) == sf::st_crs(seg_sf))

    ref_sf <- suppressWarnings(
        sf::st_sf(ref_id = seq_len(nrow(ref_sf)),
                  geometry = sf::st_geometry(ref_sf),
                  sf_column_name = "geometry")
    )

    seg_sf <- suppressWarnings(
        sf::st_sf(seg_id = seq_len(nrow(seg_sf)),
                  geometry = sf::st_geometry(seg_sf),
                  sf_column_name = "geometry")
    )

    class(ref_sf) <- unique(c("ref_sf", class(ref_sf)))
    class(seg_sf) <- unique(c("seg_sf", class(seg_sf)))

    .env <- environment()

    structure(list(),
              .env = .env,
              class = c("segmetric"))
}

#' @export
#' @rdname segmetric_functions
sm_clear <- function(m) {

    subsets <- sm_list(m)
    subsets <- subsets[!subsets %in% c("ref_sf", "seg_sf")]
    rm(list = subsets, envir = .segmetric_env(m), inherits = FALSE)
    m
}

#' @exportS3Method
print.segmetric <- function(x, ...) {
    print(c(x))
}

# type = "base"
# type = "choropleth"
# type = "subsets"

#' @exportS3Method
plot.segmetric <- function(x, type = "base", ...,
                           title = NULL,
                           background = "#FAFAFA",
                           plot_centroids = TRUE,
                           ref_symbol = 2,
                           seg_symbol = 3,
                           centroids_color = "#000000",
                           show_legend = TRUE,
                           ref_label = "reference",
                           seg_label = "segment",
                           centroids_label = "centroid",
                           ref_color = "#0000B3",
                           seg_color = "#FFF50A",
                           subset_color = "#F0E417",
                           fill_alpha = 0.2,
                           layers = c("ref_sf", "seg_sf"),
                           metric_id = NULL,
                           subset_id = NULL,
                           extent = NULL) {

    mod_alpha <- function(x, alpha) {
        if (alpha < 0) alpha <- 0
        if (alpha > 1) alpha <- 1
        alpha <- as.hexmode(round(255 * alpha, 0))
        gsub("^(#[0-9a-fA-F]{6}).*$", paste0("\\1", alpha), x)
    }

    mod_extent <- function(x, factor) {
        x[[2]] <- x[[2]] - (x[[4]] - x[[2]]) * factor
        x
    }

    if (type %in% c("base", "subset")) {

        if (!is.character(title))
            title <- NULL

        # prepare format parameters
        labels <- c()
        fill <- c()
        border <- c()
        symbols <- c()
        symbols_color <- c()

        # prepare data layers
        if (all(c("ref_sf", "seg_sf") %in% layers)) {

            ref_sf <- sm_ref(x)[-1]
            ref_sf[["type"]] <- 1
            seg_sf <- sm_seg(x)[-1]
            seg_sf[["type"]] <- 2
            data <- rbind(ref_sf, seg_sf)

            labels <- c(ref_label, seg_label)
            fill <- mod_alpha(c(ref_color, seg_color), fill_alpha)
            border <- c(ref_color, seg_color)
            symbols <- c(NA, NA)
            symbols_color <- c(NA, NA)

        } else if ("ref_sf" %in% layers) {

            ref_sf <- sm_ref(x)[-1]
            ref_sf[["type"]] <- 1
            data <- ref_sf

            labels <- c(ref_label)
            fill <- mod_alpha(ref_color, fill_alpha)
            border <- c(ref_color)
            symbols <- c(NA)
            symbols_color <- c(NA)

        } else if ("seg_sf" %in% layers) {

            seg_sf <- sm_seg(x)[-1]
            seg_sf[["type"]] <- 1
            data <- seg_sf

            labels <- c(seg_label)
            fill <- mod_alpha(seg_color, fill_alpha)
            border <- c(seg_color)
            symbols <- c(NA)
            symbols_color <- c(NA)

        } else {
            stop("Invalid layers parameter")
        }

        if (is.null(extent))
            extent <- mod_extent(sf::st_bbox(data), 0.2)

        # main plot
        plot(data,
             main = title,
             col = fill[data[["type"]]],
             border = border[data[["type"]]],
             bg = background,
             extent = extent,
             axes = TRUE,
             reset = FALSE)

        # plot centroids
        if (plot_centroids) {

            # prepare data layers
            if (all(c("ref_sf", "seg_sf") %in% layers)) {

                labels <- c(labels, paste(labels, centroids_label))
                fill <- c(fill, NA, NA)
                border <- c(border, NA, NA)
                symbols <- c(symbols, ref_symbol, seg_symbol)
                symbols_color <- c(symbols_color, centroids_color, centroids_color)

            } else if ("ref_sf" %in% layers) {

                labels <- c(labels, paste(labels, centroids_label))
                fill <- c(fill, NA)
                border <- c(border, NA)
                symbols <- c(symbols, ref_symbol)
                symbols_color <- c(symbols_color, centroids_color)

            } else if ("seg_sf" %in% layers) {

                labels <- c(labels, paste(labels, centroids_label))
                fill <- c(fill, NA)
                border <- c(border, NA)
                symbols <- c(symbols, seg_symbol)
                symbols_color <- c(symbols_color, centroids_color)

            }

            plot(sf::st_centroid(sf::st_geometry(ref_sf)),
                 pch = ref_symbol,
                 col = centroids_color,
                 lwd = 1,
                 add = TRUE)

            plot(sf::st_centroid(sf::st_geometry(seg_sf)),
                 pch = seg_symbol,
                 col = centroids_color,
                 lwd = 1,
                 add = TRUE)

        }

        if (type == "subset") {

            if (!sm_exists(x, subset_id = subset_id))
                stop(paste0("subset '", subset_id, "' not found"))

            data <- sm_subset(x, subset_id = subset_id)

            labels <- c(labels, subset_id)
            fill <- c(fill, mod_alpha(subset_color, fill_alpha))
            border <- c(border, subset_color)
            symbols <- c(symbols, NA)
            symbols_color <- c(symbols_color, NA)

            plot(sf::st_geometry(data),
                 col    = subset_color,
                 border = NA,
                 add    = TRUE)
        }

        if (show_legend) {
            graphics::legend(
                "bottom",
                legend = labels,
                fill = fill,
                border = border,
                pch = symbols,
                col = symbols_color,
                ncol = 2,
                bty = "n",
                bg = NA)
        }

    } else if (type == "choropleth") {
        s_lst <- sm_metric_subset(x, metric_id = metric_id)
        for (m_name in names(s_lst)) {
            nbreaks <- min(10, nrow(s_lst[[m_name]]))
            nbreaks <- max(nbreaks, ceiling(log2(nrow(s_lst[[m_name]]))))
            plot(
                s_lst[[m_name]][, m_name],
                main = paste(.db_get(m_name)[["name"]], m_name, sep = " - "),
                breaks = quantile(s_lst[[m_name]][[ m_name]],
                                  probs = seq(0, 1, length.out = nbreaks + 1)),
                pal = hcl.colors(nbreaks)
            )
        }
    }
}

#' @exportS3Method
#' @rdname segmetric_functions
summary.segmetric <- function(object, ...) {

    stopifnot(inherits(object, "segmetric"))

    value <- vapply(object, mean, numeric(1), ...)
    if (length(object) <= 1)
        return(unname(value))
    value
}

#' @export
#' @rdname segmetric_functions
sm_is_empty <- function(m) {

    result <- vapply(m, function(x) {
        is.null(x) || length(x) == 0 ||
            (length(x) == 1 && (is.nan(x) || is.na(x)))
    }, logical(1))

    if (length(result) <= 1)
        return(unname(result))

    result
}


#' @export
#' @rdname segmetric_functions
`[.segmetric` <- function(x, i) {
    .segmetric_check(x)
    stopifnot(all(i %in% names(x)))
    structure(
        c(x)[i],
        .env = .segmetric_env(x),
        class = c("segmetric")
    )
}
