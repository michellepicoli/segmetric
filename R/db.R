
.db_list <- function() {
    ls(.db_env)
}

.db_set <- function(metric, value) {
    assign(metric, value, envir = .db_env)
}

.db_get <- function(metric) {
    get(metric, envir = .db_env, inherits = FALSE)
}
# 
# 
# .db_get <- function(key = NULL) {
# 
#     if (is.null(key))
#         return(env[["db_m"]])
#     
#     stopifnot(length(key) == 1)
#     stopifnot(key %in% names(env[["db_m"]]))
#     env[["db_m"]][[key]]
# }
# 
# .db_set <- function(values) {
# 
#     stopifnot(is.list(values))
#     if (length(values) > 0) {
#         stopifnot(!is.null(names(values)))
#         stopifnot(all(sapply(names(dots), nchar) > 0))
#     }
#     env[["db_m"]] <- values
# }

#' @export
list_metrics <- function() {
    
    .db_list()
}

#' @export
new_metric <- function(fn, 
                       name = "", 
                       description = "", 
                       reference = "") {
    
    stopifnot(inherits(fn, "function"))
    stopifnot(is.character(name))
    stopifnot(is.character(description))
    stopifnot(is.character(citation))
    
    
    structure(
        list(fn = fn,
             name = name,
             description = description,
             reference = reference),
        class = c("metric_entry", "list"))
}

#' @export
register_metrics <- function(...) {
    
    dots <- list(...)
    
    stopifnot(!is.null(names(dots)))
    stopifnot(all(sapply(names(dots), nchar) > 0))
    stopifnot(all(sapply(dots, inherits, "metric_entry")))
    stopifnot(all(!names(dots) %in% .db_list()))
    
    for (metric in names(dots))
        .db_set(metric, value = dots[[metric]])
    
    invisible(NULL)
}

get_metric <- function(metric) {

    stopifnot(is.character(metric))
    metric <- metric[[1]]
    stopifnot(metric %in% .db_list())
    
    .db_get(metric = metric)
}

#' @exportS3Method
print.metric_entry <- function(metric) {
    
    stopifnot(is.character(metric))
    metric <- metric[[1]]
    stopifnot(metric %in% .db_list())
    
    f <- .db_get(metric = metric)
    cat(paste("-", metric, paste0("(", f[["name"]], ")")), fill = TRUE)
    cat(paste(f[["description"]]), fill = TRUE)
    cat(paste("citation:", f[["citation"]]), fill = TRUE)
}
