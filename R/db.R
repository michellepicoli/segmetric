#' @title Database of metrics functions
#' 
#' @name db_functions
#' 
#' @description 
#' These functions manipulate the database of metrics.
#' * `sm_list_metrics()` list all registered metrics.
#' * `sm_new_metric()` create a new metric entry to be registered in the database.
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
#'     id = "Example",
#'     metric = sm_new_metric(
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
#' describe_metric("Example")
#' list_metrics()
#' 
NULL

.db_list <- function() {
    ls(.db_env)
}

.db_set <- function(key, value) {
    assign(key, value, envir = .db_env)
}

.db_del <- function(key) {
    rm(list = key, envir = .db_env)
}

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
