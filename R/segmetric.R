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
              class = "segmetric")
}

#' @export
#' @rdname segmetric_functions
sm_clear <- function(m) {

    subsets <- sm_list(m)
    subsets <- subsets[!subsets %in% c("ref_sf", "seg_sf")]
    rm(list = subsets, envir = .segmetric_env(m), inherits = FALSE)
    structure(list(),
              .env = .segmetric_env(m),
              class = "segmetric")
}

#' @exportS3Method
print.segmetric <- function(x, ...) {
    print(c(x))
}

#' @title Plot function
#'
#' @name plot
#'
#' @description
#' Plot a segmetric map according to the parameter `type`:
#' * `"base"`: simple plot of the reference or segmentation polygons;
#' * `"subset"`: plot polygons from a subset over the base plot;
#' * `"choropleth"`: plot a choropleth map from polygons of a subset using 
#' metric values. 
#'
#' @param x A `segmetric` object.
#' @param type A `character`. Either `"base"`, `"subset"`, or `"choropleth"`.
#' @param ... Ignored.
#' @param title A `character` with plot title
#' @param layers A `character`. One or both of `"ref_sf"` and `"seg_sf"`
#' (works only for `type = "base"` and `type = "subset"`).
#' @param ref_color,seg_color,ref_fill,seg_fill A `character` with a 
#' valid hexadecimal color in `rgb` or `rgba` format. Set the border and fill 
#' colors for reference and segmentation polygons.
#' @param ref_label,seg_label,centroids_label A `character` with legend 
#' labels for reference polygons, segmentation polygons, and centroids.
#' @param ref_size,seg_size A `numeric`. Set symbol's size for centroids.
#' @param ref_symbol,seg_symbol An `integer`. Symbol to represent polygons' 
#' centroids (see `pch` param in \link[graphics]{points}).
#' @param selected_fill A `character` with a valid hexadecimal color in `rgb` 
#' or `rgba` format. Set the fill color of selected reference or 
#' segmentation polygons depending on `subset_id`.
#' @param plot_centroids A `logical`. Plot centroids or not.
#' @param centroids_color  A `character` with a valid hexadecimal color 
#' in `rgb` or `rgba` format. Set the border colors for centroids.
#' @param subset_id A `character` with subset name
#' (required for `type = "subset"`) 
#' @param subset_color,subset_fill A `character` with a valid hexadecimal 
#' color in `rgb` or `rgba` format. Set the border and fill 
#' colors for subset polygons (works only with `type = "subset"`).
#' @param metric_id A `character` with metric to be plotted in choropleth maps
#' (required for `type = "choropleth"`) 
#' @param break_style A `character` with the name of a method to compute 
#' the intervals for choropleth maps. Can be one of `"sd"`, `"equal"`, 
#' `"pretty"`, `"quantile"`, `"kmeans"`, `"hclust"`, `"bclust"`, `"fisher"`, 
#' `"jenks"`, `"dpih"`, and `"headtails"` (see `style` parameter in 
#' \link[classInt]{classIntervals}).
#' @param plot_extent A `sf` object. Set the map extent for a plot.
#' @param plot_legend A `logical`. Plot legend or not.
#' @param plot_axes A `logical`. Plot coordinates axis or not.
#'
#' @exportS3Method 
plot.segmetric <- function(x, type = "base", ...,
                           title = NULL,
                           layers = c("ref_sf", "seg_sf"),
                           ref_color = "#FF00009F",
                           ref_fill = "#FFFFFF00",
                           ref_label = "reference",
                           ref_size = 2,
                           ref_symbol = 2,
                           seg_color = "#0000009F",
                           seg_fill = "#FFFFFF00",
                           seg_label = "segment",
                           seg_size = 1,
                           seg_symbol = 3,
                           selected_fill = "#9A9AFF50",
                           plot_centroids = TRUE,
                           centroids_color = "#000000FF",
                           centroids_label = "centroid",
                           subset_id = NULL,
                           subset_color = "#FFFFFF00",
                           subset_fill = "#F0E4167F",
                           metric_id = NULL,
                           break_style = "jenks",
                           plot_extent = NULL,
                           plot_legend = TRUE,
                           plot_axes = TRUE) {


    mod_extent <- function(x, factor) {
        x[[2]] <- x[[2]] - (x[[4]] - x[[2]]) * factor
        x
    }

    if (type == "base") {

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
            fill <- c(ref_fill, seg_fill)
            border <- c(ref_color, seg_color)
            symbols <- c(NA, NA)
            symbols_color <- c(NA, NA)
            size <- c(ref_size, seg_size)

        } else if ("ref_sf" %in% layers) {

            ref_sf <- sm_ref(x)[-1]
            ref_sf[["type"]] <- 1
            data <- ref_sf

            labels <- c(ref_label)
            fill <- ref_fill
            border <- c(ref_color)
            symbols <- c(NA)
            symbols_color <- c(NA)
            size <- ref_size

        } else if ("seg_sf" %in% layers) {

            seg_sf <- sm_seg(x)[-1]
            seg_sf[["type"]] <- 1
            data <- seg_sf

            labels <- c(seg_label)
            fill <- seg_fill
            border <- c(seg_color)
            symbols <- c(NA)
            symbols_color <- c(NA)
            size <- seg_size

        } else {
            stop("Invalid layers parameter")
        }

        # adjust plot spatial extent
        if (is.null(plot_extent))
            plot_extent <- sf::st_bbox(data)
        else
            plot_extent <- sf::st_bbox(plot_extent)
        
        if (plot_legend)
            plot_extent <- mod_extent(plot_extent, factor = 0.167)

        # main plot
        plot(data,
             main = title,
             col = fill[data[["type"]]],
             border = border[data[["type"]]],
             lwd = size[data[["type"]]],
             extent = plot_extent,
             axes = plot_axes,
             reset = FALSE)

        # plot centroids
        if (plot_centroids) {

            # prepare data layers
            if (all(c("ref_sf", "seg_sf") %in% layers)) {

                labels <- c(labels, paste(labels, centroids_label))
                fill <- c(fill, NA, NA)
                border <- c(border, NA, NA)
                symbols <- c(symbols, ref_symbol, seg_symbol)
                symbols_color <- c(symbols_color, centroids_color,
                                   centroids_color)

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

        if (plot_legend) {
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

    } else if (type == "subset") {

        if (!sm_exists(x, subset_id = subset_id))
            stop(paste0("subset '", subset_id, "' not found"))

        subset <- sm_subset(x, subset_id = subset_id)

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
            fill <- c(ref_fill, seg_fill)
            border <- c(ref_color, seg_color)
            symbols <- c(NA, NA)
            symbols_color <- c(NA, NA)
            size <- c(ref_size, seg_size)

        } else if ("ref_sf" %in% layers) {

            ref_sf <- sm_ref(x)[-1]
            ref_sf[["type"]] <- 1
            data <- ref_sf

            labels <- c(ref_label)
            fill <- ref_fill
            border <- c(ref_color)
            symbols <- c(NA)
            symbols_color <- c(NA)
            size <- ref_size

        } else if ("seg_sf" %in% layers) {

            seg_sf <- sm_seg(x)[-1]
            seg_sf[["type"]] <- 1
            data <- seg_sf

            labels <- c(seg_label)
            fill <- seg_fill
            border <- c(seg_color)
            symbols <- c(NA)
            symbols_color <- c(NA)
            size <- seg_size

        } else {
            stop("Invalid layers parameter")
        }

        # adjust plot spatial extent
        if (is.null(plot_extent))
            plot_extent <- sf::st_bbox(data)
        else
            plot_extent <- sf::st_bbox(plot_extent)

        if (plot_legend)
            plot_extent <- mod_extent(plot_extent, factor = 0.167)

        # main plot
        plot(data,
             main = title,
             col = fill[data[["type"]]],
             border = border[data[["type"]]],
             lwd = size[data[["type"]]],
             extent = plot_extent,
             axes = plot_axes,
             reset = FALSE)

        # prepare plot of the subset
        if (colnames(subset)[2] == "ref_id") {

            ref_sf <- sm_ref(x)[-1]
            ref_sf[["type"]] <- length(fill) + 1
            rows <- unique(sm_inset(sm_ref(x), subset, return_index = TRUE))
            ref_sf <- ref_sf[rows, ]
            data <- ref_sf

            labels <- c(labels, paste("selected", ref_label))
            fill <- c(fill, selected_fill)
            border <- c(border, ref_color)
            symbols <- c(symbols, NA)
            symbols_color <- c(symbols_color, NA)
            size <- c(size, ref_size)

        } else if (colnames(subset)[2] == "seg_id") {

            seg_sf <- sm_seg(x)[-1]
            seg_sf[["type"]] <- length(fill) + 1
            rows <- unique(sm_inset(sm_seg(x), subset, return_index = TRUE))
            seg_sf <- seg_sf[rows, ]
            data <- seg_sf

            labels <- c(labels, paste("selected", seg_label))
            fill <- c(fill, selected_fill)
            border <- c(border, seg_color)
            symbols <- c(symbols, NA)
            symbols_color <- c(symbols_color, NA)
            size <- c(size, seg_size)

        } else {
            stop("Invalid subset value")
        }

        # Plot of the subset.
        plot(data,
             col = fill[data[["type"]]],
             border = border[data[["type"]]],
             lwd = size[data[["type"]]],
             add = TRUE
        )

        # plot centroids
        if (plot_centroids) {

            # prepare data layers
            if (all(c("ref_sf", "seg_sf") %in% layers)) {

                labels <- c(labels, paste(c(ref_label, seg_label),
                                          centroids_label))
                fill <- c(fill, NA, NA)
                border <- c(border, NA, NA)
                symbols <- c(symbols, ref_symbol, seg_symbol)
                symbols_color <- c(symbols_color, centroids_color,
                                   centroids_color)

            } else if ("ref_sf" %in% layers) {

                labels <- c(labels, paste(ref_label, centroids_label))
                fill <- c(fill, NA)
                border <- c(border, NA)
                symbols <- c(symbols, ref_symbol)
                symbols_color <- c(symbols_color, centroids_color)

            } else if ("seg_sf" %in% layers) {

                labels <- c(labels, paste(seg_label, centroids_label))
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

        # Plot intersection
        data <- sm_subset(x, subset_id = subset_id)
        plot(sf::st_geometry(data),
             col    = subset_fill,
             border = subset_color,
             add    = TRUE)

        if (plot_legend) {

            labels <- c(labels, "intersection")
            fill <- c(fill, subset_fill)
            border <- c(border, subset_color)
            symbols <- c(symbols, NA)
            symbols_color <- c(symbols_color, NA)

            graphics::legend(
                x = "bottom",
                legend = labels,
                fill = fill,
                border = border,
                pch = symbols,
                col = symbols_color,
                ncol = 2,
                bty = "n",
                bg = NA
            )
        }

    } else if (type == "choropleth") {
        
        stopifnot(requireNamespace("classInt"))
        
        supported_styles <- c("sd", "equal", "pretty", 
                              "quantile", "kmeans", "hclust", 
                              "bclust", "fisher", "jenks", 
                              "dpih", "headtails")
        
        break_style <- break_style[[1]]
        stopifnot(break_style %in% supported_styles)
        
        # Compute metrics
        x <- sm_compute(x, metric_id = metric_id)
        s_lst <- sm_metric_subset(round(x), metric_id = metric_id)
        for (m_name in names(s_lst)) {
            
            nbreaks <- max(
                min(10, nrow(s_lst[[m_name]])),
                ceiling(log2(nrow(s_lst[[m_name]])))
            )
            breaks <- unique(
                classInt::classIntervals(
                    var = s_lst[[m_name]][[ m_name]],
                    n = nbreaks,
                    style = break_style)$brks
            )
            
            if (is.null(title))
                title <- .db_get(m_name)[["name"]]
            
            plot(
                s_lst[[m_name]][, m_name],
                main = title,
                breaks = breaks,
                pal = hcl.colors(
                    length(breaks) - 1, 
                    rev = .db_get(m_name)[["optimal"]] == 0),
                axes = plot_axes
            )
            
            # reset title to get next metric name
            title <- NULL
        }

    }
}

#' @exportS3Method
round.segmetric <- function(x, digits = 8) {
    val <- lapply(x, round, digits = digits)
    structure(val,
              .env = .segmetric_env(x),
              class = c("segmetric"))
}

#' @exportS3Method
#' @rdname segmetric_functions
summary.segmetric <- function(object, weight = NULL, ...) {

    stopifnot(inherits(object, "segmetric"))

    value <- vapply(object, weighted.mean, numeric(1), ...)
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

