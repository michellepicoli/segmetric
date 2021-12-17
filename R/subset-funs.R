#' @title Set functions
#' 
#' @name set_functions
#' 
#' @description 
#' These functions compute subsets required to calculate segmentation metrics as
#' described in Clinton et al. (2010) and Costa et al. (2017).
#' * `sm_ref()`    returns the set of \eqn{n} polygons of reference, represented by \eqn{X = \{x_{i}: i = 1, ....., n\}}
#' * `sm_seg()`    returns the set of \eqn{m} segmentation polygons, represented by \eqn{Y = \{y_{j}: j = 1, ....., m\}}
#' * `sm_ytilde()` returns \eqn{\tilde{Y}_{i}}, a subset of \eqn{Y}, where \eqn{\tilde{Y}_{i} = \{y_{j}: \rm{area}(x_{i} \cap y_{j}) \neq 0\}}
#' * `sm_xtilde()` returns \eqn{\tilde{X}_{j}}, a subset of \eqn{X}, where \eqn{\tilde{X}_{j} = \{x_{i}: \rm{area}(y_{j} \cap x_{i}) \neq 0\}}
#' * `sm_yprime()` returns \eqn{Y'_{i}}, a subset of \eqn{Y}, where \eqn{Y'_{i} = \{y_{j}: max(\rm{area}(x_{i} \cap y_{j}))\}}
#' * `sm_xprime()` returns \eqn{X'_{j}}, a subset of \eqn{X}, where \eqn{X'_{j} = \{x_{i}: max(\rm{area}(y_{j} \cap x_{i}))\}}
#' * `sm_ya()`     returns \eqn{Y\!a_{i}}, a subset of \eqn{\tilde{Y}_{i}}, where \eqn{Y\!a_{i} = \{y_{j}: \rm{centroid}(x_{i}) \:\rm{in}\: y_{j}\}}
#' * `sm_yb()`     returns \eqn{Y\!b_{i}}, a subset of \eqn{\tilde{Y}_{i}}, where \eqn{Y\!b_{i} = \{y_{j}: \rm{centroid}(y_{j}) \:\rm{in}\: x_{i}\}}
#' * `sm_yc()`     returns \eqn{Y\!c_{i}}, a subset of \eqn{\tilde{Y}_{i}}, where \eqn{Y\!c_{i} = \{y_{j}: \rm{area}(x_{i} \cap y_{j}) / \rm{area}(y_{j}) > 0.5\}}
#' * `sm_yd()`     returns \eqn{Y\!d_{i}}, a subset of \eqn{\tilde{Y}_{i}}, where \eqn{Y\!d_{i} = \{y_{j}: \rm{area}(x_{i} \cap y_{j}) / \rm{area}(x_{i}) > 0.5\}} 
#' * `sm_ystar()`  returns \eqn{{Y}^{*}_{i}}, where \eqn{{Y}^{*}_{i} = Y\!a_{i} \cup Y\!b_{i} \cup Y\!c_{i} \cup Y\!c_{i}}  
#' * `sm_ycd()`    returns \eqn{Y\!cd_{i}}, where \eqn{Y\!cd_{i} = Y\!c_{i} \cup Y\!d_{i}} 
#' * `sm_ye()`     returns \eqn{Y\!e_{i}}, a subset of \eqn{\tilde{Y}_{i}} , where \eqn{Y\!e_{i} = \{y_{j}: \rm{area}(x_{i} \cap y_{j}) / \rm{area}(y_{j}) = 1\}} 
#' * `sm_yf()`     returns \eqn{Y\!f_{i}}, a subset of \eqn{\tilde{Y}_{i}} , where \eqn{Y\!f_{i} = \{y_{j}: \rm{area}(x_{i} \cap y_{j}) / \rm{area}(y_{j}) > 0.55\}} 
#' * `sm_yg()`     returns \eqn{Y\!g_{i}}, a subset of \eqn{\tilde{Y}_{i}} , where \eqn{Y\!g_{i} = \{y_{j}: \rm{area}(x_{i} \cap y_{j}) / \rm{area}(y_{j}) > 0.75\}} 
#' 
#' @param m      A `segmetric` object.
#' 
#' @references
#' Clinton, N., Holt, A., Scarborough, J., Yan, L., & Gong, P. (2010). Accuracy 
#' Assessment Measures for Object-based Image Segmentation Goodness. 
#' Photogrammetric Engineering & Remote Sensing, 76(3), 289–299. 
#' \doi{10.14358/PERS.76.3.289}.
#' 
#' Costa, H., Foody, G. M., & Boyd, D. S. (2018). Supervised methods of image 
#' segmentation accuracy assessment in land cover mapping. Remote Sensing of 
#' Environment, 205(December 2017), 338–351. \doi{10.1016/j.rse.2017.11.024}.
NULL

#' @rdname set_functions
#' @export
sm_ytilde <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "Y_tilde", 
        expr = {
            sm_intersection(s1 = sm_ref(m), s2 = sm_seg(m), touches = FALSE)
        }
    )
}

#' @rdname set_functions
#' @export
sm_xtilde <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "X_tilde", 
        expr = {
            sm_intersection(sm_seg(m), 
                            sm_ref(m), 
                            touches = FALSE)
        }
    )
}

#' @rdname set_functions
#' @export
sm_yprime <- function(m) {
    # to avoid warnings during check
    ref_id <- NULL
    geometry <- NULL
    
    sm_subset(
        m = m, 
        subset_id = "Y_prime", 
        expr = {
            suppressWarnings(suppressMessages(
                sm_ytilde(m) %>%
                    dplyr::group_by(ref_id) %>%
                    dplyr::slice_max(sf::st_area(geometry)) %>% 
                    dplyr::ungroup()
            ))
        }
    )
}

#' @rdname set_functions
#' @export
sm_xprime <- function(m) {
    # to avoid warnings during check
    seg_id <- NULL
    geometry <- NULL
    
    sm_subset(
        m = m, 
        subset_id = "X_prime", 
        expr = {
            suppressWarnings(suppressMessages(
                sm_xtilde(m) %>%
                    dplyr::group_by(seg_id) %>%
                    dplyr::slice_max(sf::st_area(geometry)) %>%
                    dplyr::ungroup()
            ))
        }
    )
}

#' @rdname set_functions
#' @export
sm_ya <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "Y_a", 
        expr = {
            Y <- sm_ytilde(m)
            suppressWarnings(
                sm_inset(Y, sm_intersection(s1 = sm_centroid(sm_ref(m)), 
                                            s2 = sm_seg(m),
                                            touches = TRUE))
            )
        }
    )
}

#' @rdname set_functions
#' @export
sm_yb <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "Y_b", 
        expr = {
            Y <- sm_ytilde(m)
            suppressWarnings(
                sm_inset(Y, sm_intersection(s1 = sm_centroid(sm_seg(m)), 
                                            s2 = sm_ref(m),
                                            touches = TRUE))
            )
        }
    )
}

#' @rdname set_functions
#' @export
sm_yc <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "Y_c", 
        expr = {
            Y <- sm_ytilde(m)
            suppressWarnings(
                Y[sm_area(Y) / sm_area(sm_seg(m), order = Y) > 0.5,]
            )
        }
    )
}

#' @rdname set_functions
#' @export
sm_yd <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "Y_d", 
        expr = {
            Y <- sm_ytilde(m)
            suppressWarnings(
                Y[sm_area(Y) / sm_area(sm_ref(m), order = Y) > 0.5,]
            )
        }
    )
}

#' @rdname set_functions
#' @export
sm_ystar <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "Y_star", 
        expr = {
            sm_rbind(sm_ya(m), sm_yb(m), sm_yc(m), sm_yd(m))
        }
    )
}

#' @rdname set_functions
#' @export
sm_ycd <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "Y_cd", 
        expr = {
            sm_rbind(sm_yc(m), sm_yd(m))
        }
    )
}

#' @rdname set_functions
#' @export
sm_ye <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "Y_e", 
        expr = {
            Y <- sm_ytilde(m)
            suppressWarnings(
                Y[sm_area(Y) / sm_area(sm_seg(m), order = Y) == 1,]
            )
        }
    )
}

#' @rdname set_functions
#' @export
sm_yf <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "Y_f", 
        expr = {
            Y <- sm_ytilde(m)
            suppressWarnings(
                Y[sm_area(Y) / sm_area(sm_seg(m), order = Y) == 0.55,]
            )
        }
    )
}

#' @rdname set_functions
#' @export
sm_yg <- function(m) {
    sm_subset(
        m = m, 
        subset_id = "Y_g", 
        expr = {
            Y <- sm_ytilde(m)
            suppressWarnings(
                Y[sm_area(Y) / sm_area(sm_seg(m), order = Y) == 0.75,]
            )
        }
    )
}
