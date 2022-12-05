#' 
#' # Demo 2: 
#' 
#' Study area: Luis Eduardo Magalhaes municipality, 
#' Bahia state, Brazil
#' 
library(segmetric)

# list all supported metrics
sm_list_metrics()

# load data
data("ref_sf", package = "segmetric")
data("seg1000_sf", package = "segmetric")

# open data and create a segmetric object
m1000 <- sm_read(ref_sf = ref_sf, seg_sf = seg1000_sf)

# plot geometries
plot(m1000)

# compute metrics individually and plot a choropleth map
sm_compute(m1000, metric_id = "AFI") %>% 
    plot(type = "choropleth")

# compute a group of metrics and summarize them
# NOTE: warnings expected for non-summarizable metrics
sm_compute(m1000, metric_id = sm_list_metrics()) %>% 
    summary()

# compute metrics in sequence
sm_compute(m1000, metric_id = "IoU") %>% 
    sm_compute(metric_id = "US1") %>% 
    sm_compute(metric_id = "OS2") %>% 
    sm_compute(metric_id = "PI") %>% 
    summary()

# list all already computed subsets in m object
sm_list(m1000)
