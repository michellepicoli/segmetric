
#' Gather the required information for computing the metrics.
#' 
#' @param ref_sf An sf object. The reference segmentation.
#' @param seg_sf An sf object. The segmentation under test.
#' @param ref_id A character. Name of the field containing unique Ids.
#' @param seg_id A character. Name of the field containing unique Ids.
#' @param alpha  A length-one numeric. Weight used to adjust the sensitivity of 
#'               the F_measure to Over and Under Segmentation.
#' @return A data.frame
#' \itemize{
#'   \item ref_id - ID of the reference polygons.
#'   \item seg_id - ID of the segmentation polygons.           
#'   \item intersect_area - Area of the intersection between the reference and segmentation polygons.
#'   \item union_area - Area of the union between the reference and the segmentation polygons.
#'   \item ref_area - Area of the reference polygons.
#'   \item seg_area - Area of the segmentation polygons.
#'   \item centroid_in_ref - Are the centroids of the segmentation polygons inside the reference?
#'   \item rate_int_ref_area - Proportion of the intersection to the reference polygon area.
#'   \item rate_int_seg_area - Proportion of the intersection to the segmentation polygons area.
#'   \item jaccard - Jaccard index.
#'   \item oseg - Oversegmentation index.
#'   \item useg - Undersegmentation index.
#'   \item afi - Area Fit Index (AFI) index.
#' }
#' @export
get_metrics <- function(ref_sf, seg_sf, ref_id, seg_id, alpha = 0.5){
    
    # Are indexes unique?
    if (length(unique(ref_sf[[ref_id]])) != nrow(ref_sf))
        stop("The IDs of the reference data are not unique.")
    if (length(unique(seg_sf[[seg_id]])) != nrow(seg_sf))
        stop("The IDs of the segmentation data are not unique.")
    
    # Are features in the same CRS?
    if (sf::st_crs(ref_sf) != sf::st_crs(seg_sf))
        stop("The reference and segmentatin data use different coordinate 
             reference systems.")
    
    # Are features projected?
    if (sf::st_is_longlat(ref_sf) || sf::st_is_longlat(seg_sf))
        warning("The given data is not projected. The union and interpolation 
                results are approximated. (sf package warning: although 
                coordinates are longitude/latitude, st_intersection assumes that 
                they are planar)")
    
    # Compute areas.
    ref_sf[["ref_area"]] <- sf::st_area(ref_sf)
    seg_sf[["seg_area"]] <- sf::st_area(seg_sf)
    
    # Compute intersections.
    suppressMessages(suppressWarnings(
        intersection_sf <- sf::st_intersection(x = ref_sf, y = seg_sf)
    ))
    intersection_sf[["intersect_area"]] <- sf::st_area(intersection_sf)
    
    # Get a list of overlapped ids.
    intersection_df <- sf::st_set_geometry(intersection_sf[c(ref_id, seg_id, 
                                                             "intersect_area")], 
                                           NULL)
    
    # Compute unions.
    suppressMessages(suppressWarnings(
    union_area_ls <- lapply(seq_len(nrow(intersection_df)), 
                            function(i, intersection_df, ref_sf, seg_sf){
                                my_ref <- intersection_df[[ref_id]][i]
                                my_seg <- intersection_df[[seg_id]][i]
                                union_area <- compute_union(ref_sf = ref_sf, 
                                                            seg_sf = seg_sf,
                                                            ref_id = ref_id, 
                                                            seg_id = seg_id,
                                                            id_ref = my_ref, 
                                                            id_seg = my_seg)
                                res <- data.frame(my_ref, my_seg, union_area)
                                names(res) <- c(ref_id, seg_id, "union_area")
                                return(res)
                            },  intersection_df = intersection_df, 
                            ref_sf = ref_sf, 
                            seg_sf = seg_sf)
    ))
    union_df <- do.call(rbind, union_area_ls)

    # Compute centroids. 
    suppressMessages(suppressWarnings(
        seg_cent_sf <- sf::st_centroid(seg_sf[seg_id]) 
    ))
    suppressMessages(suppressWarnings(
        cent_int_df <- sf::st_set_geometry(sf::st_intersection(x = ref_sf[ref_id], 
                                                               y = seg_cent_sf), 
                                           NULL)
    ))
    cent_int_df["centroid_in_ref"] <- TRUE
    
    # Build a data.frame with the data for computing indices.
    metric_df <- merge(x = intersection_df,
                       y = union_df,
                       by = c(ref_id, seg_id),
                       all.x = TRUE,
                       sort = TRUE)
    
    # Add the areas of the original polygons.
    metric_df <- merge(x = metric_df,
                       y = sf::st_set_geometry(ref_sf[c(ref_id, "ref_area")], 
                                               NULL),
                       by = ref_id,
                       all.x = TRUE,
                       sort = TRUE)
    metric_df <- merge(x = metric_df,
                       y = sf::st_set_geometry(seg_sf[c(seg_id, "seg_area")],
                                               NULL),
                       by = seg_id,
                       all.x = TRUE,
                       sort = TRUE)
    
    # Is the segmentation centroid in the reference polygon? 
    metric_df <- merge(x = metric_df,
                       y = cent_int_df,
                       by = c(ref_id, seg_id),
                       all.x = TRUE,
                       sort = TRUE)
    metric_df[is.na(metric_df)]  <- FALSE
    
    # Compute the proportion of intersected areas.
    metric_df["rate_int_ref_area"] <- units::drop_units(metric_df$intersect_area / metric_df$ref_area) 
    metric_df["rate_int_seg_area"] <- units::drop_units(metric_df$intersect_area / metric_df$seg_area) 
    
    # Compute the metrics
    metric_df["jaccard"] <- units::drop_units(metric_df$intersect_area                  / metric_df$union_area)
    metric_df["oseg"]    <- units::drop_units(metric_df$intersect_area                  / metric_df$ref_area)
    metric_df["useg"]    <- units::drop_units(metric_df$intersect_area                  / metric_df$seg_area)
    metric_df["afi"]     <- units::drop_units((metric_df$ref_area - metric_df$seg_area) / metric_df$ref_area )
    metric_df["f_measure"] <- 1 / (alpha * (1 / 1 - metric_df$oseg) + (1 + alpha) / (1 - metric_df$useg))
    
    return(metric_df)
}

#' Helper function. Compute the area of the union of the identified geometries.
#' 
#' @param ref_sf An sf object. The reference.
#' @param seg_sf An sf object. The segmentation.
#' @param ref_id An integer. Name of the Id field in the reference object.
#' @param seg_id An integer. Name of the Id field in the segmentation object.
#' @param id_ref An integer. Id of a reference geometry.
#' @param id_seg An integer. Id of a segmentation geometry.
#' @return       A numeric.
compute_union <- function(ref_sf, seg_sf, ref_id, seg_id, id_ref, id_seg) {
    my_ref <- sf::st_geometry(ref_sf[sf::st_set_geometry(ref_sf[ref_id], NULL) == id_ref,])
    my_seg <- sf::st_geometry(seg_sf[sf::st_set_geometry(seg_sf[seg_id], NULL) == id_seg,])
    # TODO: implement case when ref and seg polygons intersect more than once.
    # Check if the given geometries are unique.
    stopifnot(nrow(my_ref) == 1)
    stopifnot(nrow(my_seg) == 1)
    union_sf <- sf::st_union(x = my_ref, y = my_seg,
                             by_feature = FALSE)
    return(sf::st_area(union_sf))
}
