#' @importFrom magrittr `%>%`
#' 
NULL

# package environment
.db_env <- new.env()

.onLoad <- function(libname, pkgname) {
    
    sm_reg_metric(
        metric_id = "OS2",
        entry = sm_new_metric(
            fn           = OS2,
            name         = "OverSegmentation",
            description  = "Values from 0 (optimal) to 1",
            reference    = "Persello and Bruzzone (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "OS1",
        entry = sm_new_metric(
            fn           = OS1,
            name         = "OverSegmentation",
            description  = "Values from 0 (optimal) to 1",
            reference    = "Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "US2",
        entry = sm_new_metric(
            fn           = US2,
            name         = "UnderSegmentation",
            description  = "Values from 0 (optimal) to 1",
            reference    = "Persello and Bruzzone (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "US1",
        entry = sm_new_metric(
            fn           = US1,
            name         = "UnderSegmentation",
            description  = "Values from 0 (optimal) to 1",
            reference    = "Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "AFI",
        entry = sm_new_metric(
            fn           = AFI,
            name         = "Area fit index",
            description  = "Optimal value: 0",
            reference    = "Lucieer and Stein (2002) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "QR",
        entry = sm_new_metric(
            fn           = QR,
            name         = "Quality rate",
            description  = "Values from 0 (optimal) to 1",
            reference    = "Weidner (2008) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "D_index",
        entry = sm_new_metric(
            fn           = D_index,
            name         = "Index D",
            description  = "Values from 0 (optimal) to 1",
            reference    = "Levine and Nazif (1982) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "precision",
        entry = sm_new_metric(
            fn           = precision,
            name         = "Precision",
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        )
    )
    sm_reg_metric(
        metric_id = "recall",
        entry = sm_new_metric(
            fn           = recall,
            name         = "Recall",
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        )
    )
    sm_reg_metric(
        metric_id = "UMerging",
        entry = sm_new_metric(
            fn           = UMerging,
            name         = "underMerging",
            description  = "Values from 0 (optimal) to 0.5",
            reference    = "Levine and Nazif (1982) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "OMerging",
        entry = sm_new_metric(
            fn           = OMerging,
            name         = "overMerging",
            description  = "Values from 0 (optimal) to 0.5",
            reference    = "Levine and Nazif (1982) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "M",
        entry = sm_new_metric(
            fn           = M,
            name         = "Match",
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Janssen and Molenaar (1995) and Feitosa et al. (2010)"
        )
        # TODO: check formula in Carleer et al. (2005)
        
    )
    sm_reg_metric(
        metric_id = "E",
        entry = sm_new_metric(
            fn           = E,
            name         = "Evaluation measure",
            description  = "Values from 0 (optimal) to 50",
            reference    = "Carleer et al. (2005)"
        )
    )
    sm_reg_metric(
        metric_id = "RAsub",
        entry = sm_new_metric(
            fn           = RAsub,
            name         = "Relative area",
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Möller et al. (2007) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "RAsuper",
        entry = sm_new_metric(
            fn           = RAsuper,
            name         = "Relative area",
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Möller et al. (2007) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "PI",
        entry = sm_new_metric(
            fn           = PI,
            name         = "Purity Index",
            description  = "Values from 0 to 1 (optimal)",
            reference    = "van Coillie et al. (2008)"
        )
    )
    sm_reg_metric(
        metric_id = "F",
        entry = sm_new_metric(
            fn           = F1,
            name         = "Fitness function",
            description  = "Optimal value: 0",
            reference    = "Costa et al. (2008)"
        )
    )
    sm_reg_metric(
        metric_id = "OS3",
        entry = sm_new_metric(
            fn           = OS3,
            name         = "OverSegmentation",
            description  = "Values from 0 (optimal) to 1",
            reference    = "Yang et al. (2014)"
        )
    )
    sm_reg_metric(
        metric_id = "US3",
        entry = sm_new_metric(
            fn           = US3,
            name         = "UnderSegmentation",
            description  = "Values from 0 (optimal) to 1",
            reference    = "Yang et al. (2014)"
        )
    )
    sm_reg_metric(
        metric_id = "ED3",
        entry = sm_new_metric(
            fn           = ED3,
            name         = "Euclidean Distance",
            description  = "Values from 0 (optimal) to 1",
            reference    = "Yang et al. (2014)"
        )
    )
    sm_reg_metric(
        metric_id = "F_measure",
        entry = sm_new_metric(
            fn           = F_measure,
            name         = "F-measure",
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        )
    )
}
