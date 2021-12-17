#' @title LEM+ dataset
#' 
#' @name ref_sf
#'
#' @format 
#' These datasets are objects of class `sf` (inherits from `tbl_df`, 
#' `tbl`, `data.frame`) with 2 variables:
#' \itemize{
#'   \item{`id`: identification}
#'   \item{`geometry`: polygons}
#' }
#' 
#' @source \url{https://doi.org/10.1016/j.dib.2020.106553}
#' 
#' @references 
#' - Oldoni, L.V., Sanches, I.D.A., Picoli, M.C.A., 
#'   Covre, R.M. and Fronza, J.G., 2020. LEM+ dataset: For 
#'   agricultural remote sensing applications. 
#'   Data in Brief, 33, p.106553.
NULL

#' @rdname ref_sf
#' 
#' @description 
#' `ref_sf`: a dataset containing field boundaries.
#'
#' @format 
#' `ref_sf`: a dataset with 195 features.
#' 
#' @examples
#' data("ref_sf", package = "segmetric")
"ref_sf"


#' @rdname ref_sf
#' 
#' @description 
#' `sample_ref_sf`: a subset of `ref_sf` dataset.
#' 
#' @format 
#' `sample_ref_sf`: a dataset with 5 features.
#' 
#' @examples 
#' data("sample_ref_sf", package = "segmetric")
"sample_ref_sf"

#' @title Segmentation dataset
#' 
#' @name seg_sf
#' 
#' @format 
#' These datasets are objects of class `sf` (inherits from `tbl_df`, 
#' `tbl`, `data.frame`) with 2 variables:
#' \itemize{
#'   \item{`id`: identification}
#'   \item{`geometry`: polygons}
#' }
#' 
#' @references 
#' - Planet Team, 2017. Planet Application Program 
#'   Interface: In Space for Life on Earth. San Francisco, 
#'   CA. https://api.planet.com.
#'   
#' - Baatz, M., Schape, A., 2000. Multiresolution 
#'   segmentation - an optimization approach for high 
#'   quality multi-scale image segmentation. In: Strobl, J., 
#'   Blaschke, T., Griesebner, G. (Eds.), Angewandte 
#'   Geographische Informations-Verarbeitung XII. 
#'   Wichmann Verlag, Karlsruhe, Germany, pp. 12-23.
NULL


#' @rdname seg_sf
#' 
#' @description
#' `seg_sf`: a dataset containing segments generated from PlanetScope image, 
#' level 3B, acquired on Feb 18, 2020, with 3.7-meter resolution (Planet Team, 
#' 2017), using the multiresolution segmentation method (Baatz and Schape, 
#' 2000). 
#' 
#' The data was post-processed using the spectral difference algorithm on band 
#' 3.
#' 
#' The polygons were simplified using the Douglas-Peucker algorithm in QGIS. 
#' 
#' Self-intersections were removed using SAGA's Polygon Self-Intersection.
#' 
#' Segmentation parameters:
#' \itemize{
#'   \item{`scale parameter`: 500}
#'   \item{`shape`: 0.9}
#'   \item{`compactness`: 0.1}
#' }
#' 
#' Spectral difference parameters:
#' \itemize{
#'   \item{`spectral difference`: 20}
#' }
#' 
#' Simplification parameter:
#' \itemize{
#'   \item{`distance`: 10-meters}
#' }
#' 
#' Only those polygons intersecting reference data with an area-perimeter ratio 
#' above 25 were selected.
#' 
#' @format 
#' `seg_sf`: a dataset with 259 features.
#' 
#' @examples 
#' data("seg_sf", package = "segmetric")
"seg_sf"

#' @rdname seg_sf
#' 
#' @description
#' `sample_seg_sf`: a subset of `seg_sf` dataset.
#' 
#' @format 
#' `sample_seg_sf`: a dataset with 6 features.
#' 
#' @examples 
#' data("sample_seg_sf", package = "segmetric")
"sample_seg_sf"
