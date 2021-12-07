test_that("Test real data", {
    
    ref <- system.file("extdata", "reference", "LEM_dataset.shp", 
                       package = "segmetric")
    
    seg <- system.file("extdata", "segmentation", "LEM_multiresolution.shp", 
                       package = "segmetric")
    
    data <- sm_read(ref_sf = ref, seg_sf = seg)

    system.time({
        data <- compute_metric(data, metric = "US1")
        data <- sm_compute(data, metric_id = "OS2")
        data <- sm_compute(data, metric_id = "PI")
    })
    
    sm_compute(data, metric_id = "RAsub") %>% 
        sm_compute(metric_id = "US1") %>% 
        sm_compute(metric_id = "OS2") %>% 
        sm_compute(metric_id = "PI") %>% 
        summary()

    
    sm_compute(data, metric_id = "RAsub") %>%  
        summary()
})
