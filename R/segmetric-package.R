#' @title segmetric
#' 
#' @name segmetric-package
#' 
#' @aliases segmetric
#' 
#' @docType package
#' 
#' @description Metrics for assessing segmentation accuracy for geospatial data.
#'
#' @section Purpose:
#'
#' The `segmetric` package provides a set of metrics for the 
#' segmentation accuracy assessment (or evaluation) of geospatial data.
#' It includes more than 20 metrics used in the literature for spatial 
#' segmentation assessment (Van Rijsbergen, 1979; Levine and Nazif, 1982; 
#' Janssen and Molenaar, 1995; Lucieer and Stein, 2002; Carleer et al., 2005; 
#' Moller et al., 2007; van Coillie et al., 2008; Costa et al., 2008; Weidner, 
#' 2008; Feitosa et al., 2010; Clinton et al. 2010; Persello and Bruzzone, 2010; 
#' Yang et al., 2014; and Zhang et al., 2015).
#' 
#' @section Extensions:
#' 
#' The `segmetric` package is extensible and provides a set of functions to 
#' ease the implementation of new metrics. See `?sm_reg_metric()` to find how 
#' new metrics are implemented.
#' 
#' @section Contributions:
#' 
#' Contribution to this package could be done at `segmetric`'s page on GitHub: 
#' <https://github.com/michellepicoli/segmetric>.
#' 
#' @references 
#' - Carleer, A.P., Debeir, O., Wolff, E., 2005. Assessment of very high spatial 
#' resolution satellite image segmentations. Photogramm. Eng. Remote. Sens. 71, 
#' 1285-1294. <http://dx.doi.org/10.14358/PERS.71.11.1285>. 
#' - Clinton, N., Holt, A., Scarborough, J., Yan, L., Gong, P., 2010. Accuracy 
#' assessment measures for object-based image segmentation goodness. Photogramm. 
#' Eng. Remote. Sens. 76, pp. 289-299. 
#' - Costa, G.A.O.P., Feitosa, R.Q., Cazes, T.B., Feijo, B., 2008. Genetic 
#' adaptation of segmentation parameters. In: Blaschke, T., Lang, S., Hay, G.J. 
#' (Eds.), Object-based Image Analysis. Springer Berlin Heidelberg, Berlin, 
#' Heidelberg, pp. 679-695. <http://dx.doi.org/10.1007/978-3-540-77058-9_37>. 
#' - Feitosa, R.Q., Ferreira, R.S., Almeida, C.M., Camargo, F.F., Costa, 
#' G.A.O.P., 2010. Similarity metrics for genetic adaptation of segmentation 
#' parameters. In: 3rd International Conference on Geographic Object-Based Image 
#' Analysis (GEOBIA 2010). The International Archives of the Photogrammetry, 
#' Remote Sensing and Spatial Information Sciences, Ghent. 
#' - Janssen, L.L.F., Molenaar, M., 1995. Terrain objects, their dynamics and 
#' their monitoring by the integration of GIS and remote sensing. IEEE Trans. 
#' Geosci. Remote Sens. 33, pp. 749-758. <http://dx.doi.org/10.1109/36.387590>. 
#' - Levine, M.D., Nazif, A.M., 1982. An experimental rule based system for 
#' testing low level segmentation strategies. In: Preston, K., Uhr, L. (Eds.), 
#' Multicomputers and Image Processing: Algorithms and Programs. Academic Press, 
#' New York, pp. 149-160. 
#' - Lucieer, A., Stein, A., 2002. Existential uncertainty of spatial objects 
#' segmented from satellite sensor imagery. Geosci. Remote. Sens. IEEE Trans. 
#' 40, pp. 2518-2521. <http://dx.doi.org/10.1109/TGRS.2002.805072>. 
#' - Moller, M., Lymburner, L., Volk, M., 2007. The comparison index: a tool for 
#' assessing the accuracy of image segmentation. Int. J. Appl. Earth Obs. 
#' Geoinf. 9, pp. 311-321. <http://dx.doi.org/10.1016/j.jag.2006.10.002>. 
#' - Persello, C., Bruzzone, L., 2010. A novel protocol for accuracy assessment 
#' in classification of very high resolution images. IEEE Trans. Geosci. Remote 
#' Sens. 48, pp. 1232-1244. <http://dx.doi.org/10.1109/TGRS.2009.2029570>.  
#' - Van Coillie, F.M.B., Verbeke, L.P.C., De Wulf, R.R., 2008. Semi-automated 
#' forest stand delineation using wavelet based segmentation of very high 
#' resolution optical imagery. In: Object-Based Image Analysis: Spatial Concepts 
#' for Knowledge-Driven Remote Sensing Applications, pp. 237-256. 
#' <http://dx.doi.org/10.1007/978-3-540-77058-9_13>. 
#' - Van Rijsbergen, C.J., 1979. Information Retrieval. Butterworth-Heinemann, 
#' London. 
#' - Weidner, U., 2008. Contribution to the assessment of segmentation quality 
#' for remote sensing applications. Int. Arch. Photogramm. Remote Sens. Spat. 
#' Inf. Sci. 37, pp. 479-484. 
#' - Yang Yang, J., Li, P., He, Y., 2014. A multi-band approach to unsupervised 
#' scale parameter selection for multi-scale image segmentation. ISPRS J. 
#' Photogramm. Remote Sens. 94, pp. 13-24. 
#' <http://dx.doi.org/10.1016/j.isprsjprs.2014.04.008>. 
#' - Zhang, X., Feng, X., Xiao, P., He, G., Zhu, L., 2015a. Segmentation quality 
#' evaluation using region-based precision and recall measures for remote 
#' sensing images. ISPRS J. Photogramm. Remote Sens. 102, pp. 73-84. 
#' <http://dx.doi.org/10.1016/j.isprsjprs.2015.01.009>.
#'
"_PACKAGE"
NULL
