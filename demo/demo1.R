#' 
#' # Demo 1: new metric
#' 
#' Study area: Luis Eduardo Magalhaes municipality, 
#' Bahia state, Brazil
#' 
library(segmetric)

# unregister metric
sm_unreg_metric(metric_id = "IoU")

# register 'IoU' metric
sm_reg_metric(
    metric_id = "IoU",
    entry = sm_new_metric(
        fn = function(m, s) {
            sm_area(sm_yprime(m)) / 
                sm_area(sm_subset_union(sm_yprime(m)))
        },
        fn_subset = sm_yprime,
        name = "Intersection over Union (also known as Jaccard Index)",
        optimal = 1,
        description = paste("Values range from 0 to 1.",
                            "Optimal value is 1"),
        reference = "Jaccard (1912); Rezatofighi et al. (2019)"
    )
)

# describes the 'IoU' metric
sm_desc_metric("IoU")

# lists all supported metrics
sm_list_metrics()

# load datasets
ref_sf <- sf::st_read(
    system.file("extdata", "data", "ref_sf.gpkg", package = "segmetric")
)
data("ref_sf", package = "segmetric")
sf::st_write(ref_sf, "ref_sf.gpkg")
seg500_sf
data("seg200_sf", package = "segmetric")
sf::st_write(seg200_sf, "seg200_sf.gpkg")

# create segmetric object
m <- sm_read(ref_sf = ref_sf, seg_sf = seg500_sf)

# compute a metric
sm_compute(m, metric = "IoU")

# clear computed subsets
sm_clear(m)

sm_compute(m, "AFI")

# lists all subsets stored in segmetric object
sm_list(m)

# if there is no subset 'my_subset' in the object 
# evaluates 'expr' and store it. 
sm_subset(m, "my_subset", expr = {
    sm_intersection(sm_ref(m), sm_seg(m))
})
# 'my_subset' is being listed 
sm_list(m)

# retrieve 'my_subset' from segmetric object
sm_get(m, "my_subset")


