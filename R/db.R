#' @title Database of metrics functions
#' 
#' @name db_functions
#' 
#' @description 
#' These functions are used to register new metrics in the `segmetric` 
#' database.
#' 
#' * `sm_list_metrics()` list all registered metrics.
#' * `sm_new_metric()` create a new metric entry to be registered in 
#' the database.
#' * `sm_reg_metric()` register a new metric in the database.
#' * `sm_unreg_metric()` remove a metric entry from the database.
#' * `sm_desc_metric()` describe a metric registered from the database.
#' 
#' @param fn          A `function` that receives a `segmetric` object and 
#'                    returns the metric values.
#' @param name        A `character` containing the metric name 
#'                    (Defaults to `""`).
#' @param description A `character` containing a description of the metric 
#'                    (Defaults to `""`).
#' @param reference   A `character` with the reference to a scientific 
#'                    literature describing the metric.
#' @param metric_id   A `character` value containing a unique metric 
#'                    identification.
#' @param entry       A `metric_entry` object returned by `new_metric()` 
#'                    function.
#'
#' @examples 
#' sm_reg_metric(
#'     metric_id = "Example",
#'     entry = sm_new_metric(
#'         fn = function(m) {
#'             sm_area(sm_ytilde(m)) / 
#'                 sm_area(sm_ref(m), order = sm_ytilde(m))
#'         },
#'         name = "Metric name example",
#'         description = paste("Values range from A to B.",
#'                             "Optimal value is C"),
#'         reference = "Author (Year)"
#'     ))
#' 
#' sm_desc_metric("Example")
#' sm_list_metrics()
#' 
NULL

#' @rdname db_functions 
#' @keywords internal
.db_list <- function() {
    ls(.db_env)
}

#' @rdname db_functions 
#' @keywords internal
.db_set <- function(key, value) {
    assign(key, value, envir = .db_env)
}

#' @rdname db_functions 
#' @keywords internal
.db_del <- function(key) {
    rm(list = key, envir = .db_env)
}

#' @rdname db_functions 
#' @keywords internal
.db_get <- function(key) {
    stopifnot(is.character(key))
    key <- key[[1]]
    stopifnot(key %in% .db_list())
    
    get(key, envir = .db_env, inherits = FALSE)
}

#' @export
#' @rdname db_functions
sm_list_metrics <- function() {
    
    .db_list()
}

#' @export
#' @rdname db_functions
sm_new_metric <- function(fn, 
                          name = "", 
                          description = "", 
                          reference = "") {
    
    stopifnot(inherits(fn, "function"))
    stopifnot(is.character(name))
    stopifnot(is.character(description))
    stopifnot(is.character(reference))
    
    structure(
        list(fn = fn,
             name = name,
             description = description,
             reference = reference),
        class = c("metric_entry", 
                  "list")
    )
}

#' @export
#' @rdname db_functions
sm_reg_metric <- function(metric_id, entry) {
    
    stopifnot(is.character(metric_id))
    stopifnot(nchar(metric_id) > 0)
    stopifnot(inherits(entry, "metric_entry"))
    stopifnot(!metric_id %in% .db_list())
    
    .db_set(metric_id, value = entry)
    
    invisible(NULL)
}

#' @export
#' @rdname db_functions
sm_unreg_metric <- function(metric_id) {
    
    stopifnot(metric_id %in% .db_list())
    
    .db_del(metric_id)
    
    invisible(NULL)
}

#' @export
#' @rdname db_functions
sm_desc_metric <- function(metric_id) {
    
    x <- .db_get(metric_id)
    
    if (nchar(x[["name"]]) > 0)
        cat(paste("*", metric_id, paste0("(", x[["name"]], ")")), fill = TRUE)
    else
        cat(paste("*", metric_id), fill = TRUE)
    
    # print function body
    cat(paste(" ", deparse(x[["fn"]])), fill = TRUE)
    
    if (nchar(x[["description"]]) > 0)
        cat(paste(" ", x[["description"]]), fill = TRUE)
    if (nchar(x[["reference"]]) > 0)
        cat(paste(" ", "reference:", x[["reference"]]), fill = TRUE)
}

.db_registry <- function() {
    
    
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
            reference    = "M\u00f6ller et al. (2007) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "RAsuper",
        entry = sm_new_metric(
            fn           = RAsuper,
            name         = "Relative area",
            description  = "Values from 0 to 1 (optimal)",
            reference    = "M\u00f6ller et al. (2007) and Clinton et al. (2010)"
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
        metric_id = "Fitness",
        entry = sm_new_metric(
            fn           = Fitness,
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