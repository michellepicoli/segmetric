test_that("Test real data", {
    
    ref <- system.file("extdata", "reference", "LEM_dataset.shp", 
                       package = "segmetric")
    
    seg <- system.file("extdata", "segmentation", "LEM_multiresolution.shp", 
                       package = "segmetric")
    
    data <- metric(ref_sf = ref, seg_sf = seg_sf)

    data <- get_metric(data, metric = "OS1")
    
    multi_metrics(data, metrics = c("US1", "OS2", "PI"))

})
