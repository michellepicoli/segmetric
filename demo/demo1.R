
# register 'Example' metric
sm_reg_metric(
    metric_id = "Example",
    entry = sm_new_metric(
        fn = function(m) {
            sm_area(sm_ytilde(m)) / 
                sm_area(sm_ref(m), order = sm_ytilde(m))
        },
        name = "Metric name example",
        description = paste("Values range from A to B.",
                            "Optimal value is C"),
        reference = "Author (Year)"
    ))

# describes the 'Example' metric
sm_desc_metric("Example")

# lists all supported metrics
sm_list_metrics()

# load datasets
data("ref_sf", package = "segmetric")
data("seg_sf", package = "segmetric")

# create segmetric object
m <- sm_read(ref_sf = ref_sf, seg_sf = seg_sf)

# compute a metric
sm_compute(m, metric = "Example")

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


