#' 
#' # Demo
#' 
#' Study area: Luis Eduardo Magalhaes municipality, 
#' Bahia state, Brazil
#' 
library(segmetric)

# list all supported metrics
sm_list_metrics()

# load data
data("ref_sf", package = "segmetric")
data("seg100_sf", package = "segmetric")
data("seg500_sf", package = "segmetric")
data("seg800_sf", package = "segmetric")

# open data and create a segmetric object
m100 <- sm_read(ref_sf = ref_sf, seg_sf = seg100_sf)
m500 <- sm_read(ref_sf = ref_sf, seg_sf = seg500_sf)
m800 <- sm_read(ref_sf = ref_sf, seg_sf = seg800_sf)


# compute metrics individually
sm_compute(m100, metric_id = "AFI") %>% 
    plot.segmetric(plot_centroids = FALSE, 
                   fill_alpha = 0.8)

sm_compute(m100, metric_id = sm_list_metrics()) %>% 
    plot()

sm_compute(m500, metric_id = sm_list_metrics()) %>% 
    summary()

sm_compute(m800, metric_id = sm_list_metrics()) %>%
    summary()

# dplyr::bind_rows(list(
#     sm_compute(m100, metric_id = sm_list_metrics()) %>% 
#         summary() %>% tibble::as_tibble_row(),
#     
#     sm_compute(m500, metric_id = sm_list_metrics()) %>% 
#         summary() %>% tibble::as_tibble_row(),
#     
#     sm_compute(m800, metric_id = sm_list_metrics()) %>%
#         summary() %>% tibble::as_tibble_row()
# )) %>% dplyr::summarise(dplyr::across(dplyr::everything(), sm_optmal))
    
# sm_analysis(
#     sm_compute(m100, metric_id = sm_list_metrics()) %>% 
#         summary() %>% tibble::as_tibble_row(),
#     sm_compute(m500, metric_id = sm_list_metrics()) %>% 
#         summary() %>% tibble::as_tibble_row(),
#     sm_compute(m800, metric_id = sm_list_metrics()) %>%
#         summary() %>% tibble::as_tibble_row()
# )

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
