#' @title Set functions
#' 
#' @name set_functions
#' 
#' @description 
#' These functions manipulate segmetric objects.
#' * `sm_ref()` Get the set of n polygons of reference, represented by \eqn{X = \{x_{i}: i = 1, ....., n\}}
#' * `sm_seg()` Get the set of m segmentation polygons, represented by \eqn{Y = \{y_{j}: j = 1, ....., m\}}
#' * `sm_ytilde()` Get the \eqn{Y} subset \eqn{\tile{Y}_{i} = \{y_{j}: area(x_{i} \cap y_{j}) \neq 0\}}
#' * `sm_xtilde()` Get the \eqn{X} subset \eqn{\tile{X}_{j} = \{x_{i}: area(y_{j} \cap x_{i}) \neq 0\}}
#' * `sm_yprime()` Get the \eqn{Y} subset \eqn{Y'_{i} = \{y_{j}: max(area(x_{i} \cap y_{j}))\}}
#' * `sm_xprime()` Get the \eqn{X} subset \eqn{X'_{j} = \{x_{i}: max(area(y_{j} \cap x_{i}))\}}
#' * `sm_ya()` Get the \eqn{\tilde{Y}_{i}$ subset $Ya_{i} = \{y_{j}: the centroid of x_{i} is in y_{j}\}}
#' * `sm_yb()` Get the \eqn{\tilde{Y}_{i}$ subset $Yb_{i} = \{y_{j}: the centroid of y_{j} is in x_{i}\}}
#' * `sm_yc()` Get the \eqn{\tilde{Y}_{i}$ subset $Yc_{i} = \{y_{j}: area(x_{i} \cap y_{j}) \div area(y_{j}) > 0.5\}}
#' * `sm_yd()` Get the \eqn{\tilde{Y}_{i}$ subset $Yd_{i} = \{y_{j}: area(x_{i} \cap y_{j}) \div area(x_{i}) > 0.5\}} 
#' * `sm_ystar()` Get the subset \eqn{\star{Y}_{i} = Ya_{i} \cup Yb_{i} \cup Yc_{i} \cup Yc_{i}}  
#' * `sm_ycd()` Get the subset \eqn{Ycd_{i} = Yc_{i} \cup Yd_{i}} 
#' * `sm_ye()` Get the \eqn{\tilde{Y}_{i}$ subset $Ye_{i} = \{y_{j}: area(x_{i} \cap y_{j}) \div area(y_{j}) = 1\}} 
#' * `sm_yf()` Get the \eqn{\tilde{Y}_{i}$ subset $Yf_{ì} = \{y_{j}: area(x_{i} \cap y_{j}) \div area(y_{j}) > 0.55\}} 
#' * `sm_yg()` Get the \eqn{\tilde{Y}_{i}$ subset $Yg_{ì} = \{y_{j}: area(x_{i} \cap y_{j}) \div area(y_{j}) > 0.75\}} 
#' 
#' @param m      A `segmetric` object.
#' 
#' @references
#' Clinton, N., Holt, A., Scarborough, J., Yan, L., & Gong, P. (2010). Accuracy Assessment Measures for Object-based Image Segmentation Goodness. Photogrammetric Engineering & Remote Sensing, 76(3), 289–299. https://doi.org/10.14358/PERS.76.3.289
#' Costa, H., Foody, G. M., & Boyd, D. S. (2018). Supervised methods of image segmentation accuracy assessment in land cover mapping. Remote Sensing of Environment, 205(December 2017), 338–351. https://doi.org/10.1016/j.rse.2017.11.024
NULL

#' @rdname set_functions
#' @export
sm_ytilde <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_tilde", 
        expr = {
            sm_intersections(s1 = sm_ref(m), s2 = sm_seg(m), touches = FALSE)
        }
    )
}

#' @rdname set_functions
#' @export
sm_xtilde <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "X_tilde", 
        expr = {
            intersection(x = sm_seg(m), y = sm_ref(m), touches = FALSE)
        }
    )
}

#' @rdname set_functions
#' @export
sm_yprime <- function(m) {
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

#' @rdname set_functions
#' @export
sm_xprime <- function(m) {
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

#' @rdname set_functions
#' @export
sm_ya <- function(m) {
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

#' @rdname set_functions
#' @export
sm_yb <- function(m) {
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

#' @rdname set_functions
#' @export
sm_yc <- function(m) {
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

#' @rdname set_functions
#' @export
sm_yd <- function(m) {
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

#' @rdname set_functions
#' @export
sm_ystar <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_star", 
        expr = {
            bind_all(Y_a(m), Y_b(m), Y_c(m), Y_d(m))
        }
    )
}

#' @rdname set_functions
#' @export
sm_ycd <- function(m) {
    sm_eval_subset(
        m = m, 
        subset = "Y_cd", 
        expr = {
            bind_all(Y_c(m), Y_d(m))
        }
    )
}

#' @rdname set_functions
#' @export
sm_ye <- function(m) {
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

#' @rdname set_functions
#' @export
sm_yf <- function(m) {
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

#' @rdname set_functions
#' @export
sm_yg <- function(m) {
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
