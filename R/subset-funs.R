#' @title Subset handling functions
#' 
#' @name subset_handling_functions
#' 
#' @description 
#' These functions manipulate segmetric objects.
#' * `sm_list_subsets()` ...
#' * `sm_eval_subset()` ...
#' * `sm_get_subset()` ...
#' 
#' @param s A `sf` object...
#' @param m      A `segmetric` object ...
#' @param subset A `character` value ...
#' @param expr A `numeric` vector ...
#' 
#' @examples 
#' 
NULL


#' @export
Y_tilde <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_tilde", 
        expr = {
            sm_intersections(s1 = sm_ref(m), s2 = sm_seg(m), touches = FALSE)
        }
    )
}

#' @export
X_tilde <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "X_tilde", 
        expr = {
            intersection(x = sm_seg(m), y = sm_ref(m), touches = FALSE)
        }
    )
}

#' @export
Y_prime <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_prime", 
        expr = {
            Y_tilde(m) %>%
                dplyr::mutate(inter_area = area(.)) %>%
                dplyr::group_by(ref_id) %>%
                dplyr::filter(inter_area == max(inter_area)) %>%
                dplyr::select(-inter_area) %>%
                dplyr::slice(1) %>%
                dplyr::ungroup()
        }
    )
}

#' @export
X_prime <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "X_prime", 
        expr = {
            X_tilde(m) %>%
                dplyr::mutate(inter_area = area(.)) %>%
                dplyr::group_by(seg_id) %>%
                dplyr::filter(inter_area == max(inter_area)) %>%
                dplyr::select(-inter_area) %>%
                dplyr::slice(1) %>%
                dplyr::ungroup()
        }
    )
}

#' @export
Y_a <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_a", 
        expr = {
            m <- Y_tilde(m)
            suppressWarnings(
                m[m %inset% intersection(m1 = centroid(sm_ref(m)), 
                                         m2 = sm_seg(m)),]
            )
        }
    )
}

#' @export
Y_b <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_b", 
        expr = {
            Y <- Y_tilde(m)
            suppressWarnings(
                Y[Y %inset% intersection(x = centroid(sm_seg(m)), 
                                         y = sm_ref(m)),]
            )
        }
    )
}

#' @export
Y_c <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_c", 
        expr = {
            Y <- Y_tilde(m)
            suppressWarnings(
                Y[area(Y) / area(sm_seg(m), order = seg_id(Y)) > 0.5,]
            )
        }
    )
}

#' @export
Y_d <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_d", 
        expr = {
            Y <- Y_tilde(m)
            suppressWarnings(
                Y[area(Y) / area(sm_ref(m), order = ref_id(Y)) > 0.5,]
            )
        }
    )
}

#' @export
Y_star <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_star", 
        expr = {
            bind_all(Y_a(m), Y_b(m), Y_c(m), Y_d(m))
        }
    )
}

#' @export
Y_cd <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_cd", 
        expr = {
            bind_all(Y_c(m), Y_d(m))
        }
    )
}

#' @export
Y_e <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_e", 
        expr = {
            Y <- Y_tilde(m)
            suppressWarnings(
                Y[area(Y) / area(sm_seg(m), order = seg_id(Y)) == 1,]
            )
        }
    )
}

#' @export
Y_f <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_f", 
        expr = {
            Y <- Y_tilde(m)
            suppressWarnings(
                Y[area(Y) / area(sm_seg(m), order = seg_id(Y)) == 0.55,]
            )
        }
    )
}

#' @export
Y_g <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_g", 
        expr = {
            Y <- Y_tilde(m)
            suppressWarnings(
                Y[area(Y) / area(sm_seg(m), order = seg_id(Y)) == 0.75,]
            )
        }
    )
}
