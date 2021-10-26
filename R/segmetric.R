#' @importFrom magrittr `%>%`
#' 
NULL

# package environment
env <- new.env()

.onLoad <- function(libname, pkgname) {
    
    register_metrics(
        OS2 = new_metric(
            fn          = OS2,
            name        = "OverSegmentation",
            citation    = "Persello and Bruzzone (2010)"
        ),
        OS1 = new_metric(
            fn          = OS1,
            citation    = "Clinton et al. (2010)"
        ),
        US2 = new_metric(
            fn         = US2,
            citation   = "Persello and Bruzzone (2010)"
        ),
        US1 = new_metric(
            fn         = US1,
            citation   = "Clinton et al. (2010)"
        ),
        AFI = new_metric(
            fn         = AFI,
            citation   = "Lucieer and Stein (2002) and Clinton et al. (2010)"
        ),
        QR = new_metric(
            fn         = QR,
            citation   = "Weidner (2008) and Clinton et al. (2010)"
        ),
        D_index = new_metric(
            fn         = D_index,
            citation   = "Levine and Nazif (1982) and Clinton et al. (2010)"
        ),
        precision = new_metric(
            fn         = precision,
            citation   = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        ),
        recall = new_metric(
            fn         = recall,
            citation   = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        ),
        UMerging = new_metric(
            fn         = UMerging,
            citation   = "Levine and Nazif (1982) and Clinton et al. (2010)"
        ),
        OMerging = new_metric(
            fn         = OMerging,
            citation   = "Levine and Nazif (1982) and Clinton et al. (2010)"
        ),
        M = new_metric(
            fn         = M,
            citation   = "Janssen and Molenaar (1995) and Feitosa et al. (2010)"
        ),
        # TODO: check formula in Carleer et al. (2005)
        E = new_metric(
            fn         = E,
            citation   = "Carleer et al. (2005)"
        ),
        RAsub = new_metric(
            fn         = RAsub,
            citation   = "Möller et al. (2007) and Clinton et al. (2010)"
        ),
        RAsuper = new_metric(
            fn         = RAsuper,
            citation   = "Möller et al. (2007) and Clinton et al. (2010)"
        ),
        PI = new_metric(
            fn         = PI,
            citation   = "van Coillie et al. (2008)"
        ),
        `F` = new_metric(
            fn         = F1,
            citation   = "Costa et al. (2008)"
        ),
        OS3 = new_metric(
            fn         = OS3,
            citation   = "Yang et al. (2014)"
        ),
        US3 = new_metric(
            fn         = US3,
            citation   = "Yang et al. (2014)"
        ),
        ED3 = new_metric(
            fn         = ED3,
            citation   = "Yang et al. (2014)"
        ),
        F_measure = new_metric(
            fn         = F_measure,
            citation   = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        )
    )
}