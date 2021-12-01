#######
Y_z <- function(m) {
    
    m
}

sm_register_metric(
    id = "Example",
    metric = sm_new_metric(
        fn = function(m) {
            sm_area(sm_ytilde(m)) / 
                sm_area(sm_ref(m), order = sm_ytilde(m))
        },
        name = "Metric name example",
        description = paste("Values range from A to B.",
                            "Optimal value is C"),
        reference = "Author (Year)"
    ))

sm_desc_metric("Example")
sm_list_metrics()


ref <- system.file("extdata", "reference", "LEM_dataset.shp", 
                   package = "segmetric")

seg <- system.file("extdata", "segmentation", "LEM_multiresolution.shp", 
                   package = "segmetric")

m <- sm_read(ref_sf = ref, seg_sf = seg)

sm_compute(data, metric = "Example")

########

ref <- system.file("extdata", "reference", "LEM_dataset.shp", 
                   package = "segmetric")

seg <- system.file("extdata", "segmentation", "LEM_multiresolution.shp", 
                   package = "segmetric")

m <- sm_read(ref_sf = ref, seg_sf = seg)

sm_list_metrics()
sm_desc_metric("AFI")

sm_compute(m, metric = "AFI") %>% summary()
sm_compute(m, metric = "OS3") %>% summary()

sm_list(m)

plot(m)
