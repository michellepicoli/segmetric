#' @title Database of metrics functions
#' 
#' @name db_functions
#' 
#' @description 
#' These functions are used to register new metrics in the `segmetric` 
#' database.
#' 
#' * `sm_list_metrics()`: List all registered metrics.
#' * `sm_new_metric()`: Create a new metric entry to be registered in 
#' the database.
#' * `sm_reg_metric()`: Register a new metric in the database.
#' * `sm_unreg_metric()`: Remove a metric entry from the database.
#' * `sm_desc_metric()`: Describe a metric registered from the database.
#' 
#' @param fn          A `function` that receives a `segmetric` object and 
#'                    returns the metric values.
#' @param fn_subset   A `function` that returns a `subset`.
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
#' @returns 
#' * `sm_list_metrics()`: Return `character` vector with supported metrics.
#' * `sm_new_metric()`: Return a `metric_entry` object containing the
#' metric function (`fn`), name (`name`), description (`description`), and 
#' reference (`reference`).
#' * `sm_reg_metric()`, `sm_unreg_metric()`: No return value, called to 
#' (un)register a metric.
#' * `sm_desc_metric()`: No return value, called to print a metric description.
#'
#' @examples 
#' sm_reg_metric(
#'     metric_id = "Example",
#'     entry = sm_new_metric(
#'         fn = function(m) {
#'             sm_area(sm_ytilde(m)) / 
#'                 sm_area(sm_ref(m), order = sm_ytilde(m))
#'         },
#'         fn_subset = sm_ytilde,
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
    # TODO: compute levenshtein distance to suggest possible metrics
    if (!key %in% .db_list()) {
        stop("metric '", key, "' not found\n",
             "use `sits_list_metrics()` to print metrics", call. = FALSE)
    }
    
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
                          fn_subset,
                          name = "", 
                          optimal = 0,
                          summarizable = TRUE,
                          description = "", 
                          reference = "") {
    
    stopifnot(inherits(fn, "function"))
    stopifnot(is.character(name))
    stopifnot(is.numeric(optimal))
    stopifnot(is.logical(summarizable))
    stopifnot(is.character(description))
    stopifnot(is.character(reference))
    
    structure(
        list(fn = fn,
             fn_subset = fn_subset,
             name = name,
             optimal = optimal,
             summarizable = summarizable,
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
            fn_subset    = sm_yprime,
            name         = "Oversegmentation",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 1",
            reference    = "Persello and Bruzzone (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "OS1",
        entry = sm_new_metric(
            fn           = OS1,
            fn_subset    = sm_ystar,
            name         = "Oversegmentation",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 1",
            reference    = "Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "US2",
        entry = sm_new_metric(
            fn           = US2,
            fn_subset    = sm_yprime,
            name         = "Undersegmentation",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 1",
            reference    = "Persello and Bruzzone (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "US1",
        entry = sm_new_metric(
            fn           = US1,
            fn_subset    = sm_ystar,
            name         = "Undersegmentation",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 1",
            reference    = "Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "AFI",
        entry = sm_new_metric(
            fn           = AFI,
            fn_subset    = sm_yprime,
            name         = "Area fit index",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Optimal value is 0",
            reference    = "Lucieer and Stein (2002) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "QR",
        entry = sm_new_metric(
            fn           = QR,
            fn_subset    = sm_ystar,
            name         = "Quality rate",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 1",
            reference    = "Weidner (2008) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "D_index",
        entry = sm_new_metric(
            fn           = D_index,
            fn_subset    = sm_ystar,
            name         = "Index D",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 1",
            reference    = "Levine and Nazif (1982) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "precision",
        entry = sm_new_metric(
            fn           = precision,
            fn_subset    = sm_xprime,
            name         = "Precision",
            optimal      = 1,
            summarizable = TRUE,
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        )
    )
    sm_reg_metric(
        metric_id = "recall",
        entry = sm_new_metric(
            fn           = recall,
            fn_subset    = sm_yprime,
            name         = "Recall",
            optimal      = 1,
            summarizable = TRUE,
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        )
    )
    sm_reg_metric(
        metric_id = "UMerging",
        entry = sm_new_metric(
            fn           = UMerging,
            fn_subset    = sm_ystar,
            name         = "underMerging",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 0.5",
            reference    = "Levine and Nazif (1982) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "OMerging",
        entry = sm_new_metric(
            fn           = OMerging,
            fn_subset    = sm_ystar,
            name         = "overMerging",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 0.5",
            reference    = "Levine and Nazif (1982) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "M",
        entry = sm_new_metric(
            fn           = M,
            fn_subset    = sm_yprime,
            name         = "Match",
            optimal      = 1,
            summarizable = TRUE,
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Janssen and Molenaar (1995) and Feitosa et al. (2010)"
        )
        # TODO: check formula in Carleer et al. (2005)
        
    )
    sm_reg_metric(
        metric_id = "E",
        entry = sm_new_metric(
            fn           = E,
            fn_subset    = sm_xprime,
            name         = "Evaluation measure",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 50",
            reference    = "Carleer et al. (2005)"
        )
    )
    sm_reg_metric(
        metric_id = "RAsub",
        entry = sm_new_metric(
            fn           = RAsub,
            fn_subset    = sm_ytilde,
            name         = "Relative area (RAsub)",
            optimal      = 1,
            summarizable = FALSE,
            description  = "Values from 0 to 1 (optimal)",
            reference    = "M\u00f6ller et al. (2007) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "RAsuper",
        entry = sm_new_metric(
            fn           = RAsuper,
            fn_subset    = sm_ytilde,
            name         = "Relative area (RAsuper)",
            optimal      = 1,
            summarizable = FALSE,
            description  = "Values from 0 to 1 (optimal)",
            reference    = "M\u00f6ller et al. (2007) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "PI",
        entry = sm_new_metric(
            fn           = PI,
            fn_subset    = sm_ytilde,
            name         = "Purity index",
            optimal      = 1,
            summarizable = TRUE,
            description  = "Values from 0 to 1 (optimal)",
            reference    = "van Coillie et al. (2008)"
        )
    )
    sm_reg_metric(
        metric_id = "Fitness",
        entry = sm_new_metric(
            fn           = Fitness,
            fn_subset    = sm_xprime,
            name         = "Fitness function",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Optimal value is 0",
            reference    = "Costa et al. (2008)"
        )
    )
    sm_reg_metric(
        metric_id = "OS3",
        entry = sm_new_metric(
            fn           = OS3,
            fn_subset    = sm_ycd,
            name         = "Oversegmentation",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 1",
            reference    = "Yang et al. (2014)"
        )
    )
    sm_reg_metric(
        metric_id = "US3",
        entry = sm_new_metric(
            fn           = US3,
            fn_subset    = sm_ycd,
            name         = "Undersegmentation",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 1",
            reference    = "Yang et al. (2014)"
        )
    )
    sm_reg_metric(
        metric_id = "ED3",
        entry = sm_new_metric(
            fn           = ED3,
            fn_subset    = sm_ycd,
            name         = "Euclidean distance",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Values from 0 (optimal) to 1",
            reference    = "Yang et al. (2014)"
        )
    )
    sm_reg_metric(
        metric_id = "F_measure",
        entry = sm_new_metric(
            fn           = F_measure,
            fn_subset    = NULL,
            name         = "F-measure",
            optimal      = 1,
            summarizable = TRUE,
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Van Rijsbergen (1979) and Zhang et al. (2015)"
        )
    )
    sm_reg_metric(
        metric_id = "IoU",
        entry = sm_new_metric(
            fn           = IoU,
            fn_subset    = sm_yprime,
            name         = paste0("Intersection over Union (also known as",
                                  "Jaccard Index)"),
            optimal      = 1,
            summarizable = TRUE,
            description  = "Values from 0 to 1 (optimal)",
            reference = "Jaccard (1912); Rezatofighi et al. (2019)"
        )
    )
    sm_reg_metric(
        metric_id = "SimSize",
        entry = sm_new_metric(
            fn           = SimSize,
            fn_subset    = sm_ystar,
            name         = "Similarity of size",
            optimal      = 1,
            summarizable = TRUE,
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Zhan et al. (2005)"
        )
    )
    sm_reg_metric(
        metric_id = "qLoc",
        entry = sm_new_metric(
            fn           = qLoc,
            fn_subset    = sm_ystar,
            name         = "Quality of object's location",
            optimal      = 0,
            summarizable = TRUE,
            description  = "Optimal value is 0",
            reference    = "Zhan et al. (2005)"
        )
    )
    sm_reg_metric(
        metric_id = "RPsub",
        entry = sm_new_metric(
            fn           = RPsub,
            fn_subset    = sm_ytilde,
            name         = "Relative position (sub)",
            optimal      = 0,
            summarizable = FALSE,
            description  = "Optimal value is 0",
            reference    = "Möller et al. (2007) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "RPsuper",
        entry = sm_new_metric(
            fn           = RPsuper,
            fn_subset    = sm_ystar,
            name         = "Relative position (super)",
            optimal      = 0,
            summarizable = FALSE,
            description  = "Values from 0 (optimal) to 1",
            reference    = "Möller et al. (2007) and Clinton et al. (2010)"
        )
    )
    sm_reg_metric(
        metric_id = "OI2",
        entry = sm_new_metric(
            fn           = OI2,
            fn_subset    = sm_ytilde,
            name         = "Overlap index",
            optimal      = 0,
            summarizable = FALSE,
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Yang et al. (2017)"
        )
    )
    sm_reg_metric(
        metric_id = "Dice",
        entry = sm_new_metric(
            fn           = Dice,
            fn_subset    = NULL,
            name         = "Sorensen–Dice coefficient",
            optimal      = 1,
            summarizable = TRUE,
            description  = "Values from 0 to 1 (optimal)",
            reference    = "Dice (1945)"
        )
    )
}