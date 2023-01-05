#' @importFrom magrittr `%>%`
#' @export
magrittr::`%>%`

#' @importFrom graphics legend
#' @importFrom grDevices hcl.colors
#' @importFrom stats quantile
#' @importFrom sf st_area st_centroid st_distance st_intersection st_union
#' read_sf st_is_valid st_crs st_geometry st_bbox st_drop_geometry st_as_sf
NULL

# package environment
.db_env <- new.env()

# package on load
.onLoad <- function(libname, pkgname) {
    sm_options(digits = 10)
    .db_registry()
}
