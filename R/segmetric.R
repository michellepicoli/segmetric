#' @title General functions
#' 
#' @name segmetric_functions
#' 
#' @description 
#' These functions manipulate `segmetric` objects.
#' * `sm_read()` Load the reference and segmentation polygons into segmetric.
#' * `sm_clear()` Remove the already calculated metrics from segmetric.
#' * `print()` Print a segmetric object.
#' * `plot()` Plot the reference and segmentation polygons.
#' * `summary()` Compute a measure of central tendency over the values of a metric.
#' * `sm_is_empty()` Check if a `segmetric` object is empty.
#' 
#' @param m A `segmetric` object.
#' @param object A `segmetric` object.
#' @param ref_sf A `sf` object. The reference polygons.
#' @param seg_sf A `sf` object. The segmentation polygons.
#' @param ...    Additional parameters (Not implemented).
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
    
    if (is.character(seg_sf))
        seg_sf <- sf::read_sf(seg_sf)
    stopifnot(inherits(seg_sf, "sf"))
    
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

#' @exportS3Method
plot.segmetric <- function(x, ..., title = NULL,
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
                           fill_alpha = 0.2) {
    
    ref_sf <- sm_ref(x)[-1]
    ref_sf[["type"]] <- 1
    seg_sf <- sm_seg(x)[-1]
    seg_sf[["type"]] <- 2
    
    data <- rbind(ref_sf, seg_sf)
    
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
    
    if (!is.character(title))
        title <- NULL
    labels <- c(ref_label, seg_label)
    fill <- mod_alpha(c(ref_color, seg_color), fill_alpha)
    border <- c(ref_color, seg_color)
    symbols <- c(NA, NA)
    symbols_color <- c(NA, NA)
    
    plot(data,
         main = title,
         col = fill[data[["type"]]],
         border = border[data[["type"]]],
         bg = background,
         extent = mod_extent(sf::st_bbox(data), 0.2),
         axes = TRUE,
         reset = FALSE)

    if (plot_centroids) {
        
        labels <- c(labels, paste(labels, centroids_label))
        fill <- c(fill, NA, NA)
        border <- c(border, NA, NA)
        symbols <- c(symbols, ref_symbol, seg_symbol)
        symbols_color <- c(symbols_color, centroids_color, centroids_color)
        
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
}

#' @exportS3Method
#' @rdname segmetric_functions
summary.segmetric <- function(object, ...) {
    
    stopifnot(inherits(object, "segmetric"))
    
    value <- sapply(object, mean, ...)
    if (length(object) <= 1)
        return(unname(value))
    value
}

#' @export
#' @rdname segmetric_functions
sm_is_empty <- function(m) {
    
    return((length(m[[1]]) == 1 && (is.na(m[[1]]) || is.nan(m[[1]]))) || 
               is.null(m[[1]]) || 
               length(m[[1]]) == 0)
}
