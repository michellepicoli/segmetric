test_that("Test real data", {
    
    ref <- system.file("extdata", "reference", "LEM_dataset.shp", 
                       package = "segmetric")
    
    seg <- system.file("extdata", "segmentation", "LEM_multiresolution.shp", 
                       package = "segmetric")
    
    data <- sm_data(ref_sf = ref, seg_sf = seg)

    system.time({
        data <- compute_metric(data, metric = "US1")
        data <- get_metric(data, metric = "OS2")
        data <- get_metric(data, metric = "PI")
    })
    
    system.time({
        multi_metrics(data, metrics = c("US1", "OS2", "PI"))
    })

})
