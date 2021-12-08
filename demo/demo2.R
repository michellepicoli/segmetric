#' 
#' # Demo
#' 
#' Study area: Luís Eduardo Magalhães municipality, 
#' Bahia state, Brazil
#' 

# load data
data("ref_sf", package = "segmetric")
data("seg_sf", package = "segmetric")

# open data and create a segmetric object
m <- sm_read(ref_sf, seg_sf)

# list all supported metrics
sm_list_metrics()

# describe a specific metric
sm_desc_metric("AFI")

# compute metrics individually
sm_compute(m, metric = "AFI") %>% summary()
sm_compute(m, metric = "OS3") %>% summary()

# compute metrics in sequence
sm_compute(m, metric_id = "RAsub") %>% 
    sm_compute(metric_id = "US1") %>% 
    sm_compute(metric_id = "OS2") %>% 
    sm_compute(metric_id = "PI") %>% 
    summary()

# list all already computed subsets in m object
sm_list(m)

# plot geometries
plot(m)
