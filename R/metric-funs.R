
OS1 <- function(m) {
    sm_norm_frac(sm_area(sm_ystar(m)), sm_area(sm_ref(m)))
}

OS1 <- function(m) {
    norm_frac(sm_area(Y_star(m)), sm_area(ref_sf(m), order = ref_id(Y_star(m))))
}

US1 <- function(m) {
    norm_frac(sm_area(Y_star(m)), sm_area(seg_sf(m), order = seg_id(Y_star(m))))
}

OS2 <- function(m) {
    norm_frac(sm_area(Y_prime(m)), sm_area(ref_sf(m), order = ref_id(Y_prime(m))))
}

US2 <- function(m) {
    norm_frac(sm_area(Y_prime(m)), sm_area(seg_sf(m), order = seg_id(Y_prime(m))))
}

OS3 <- function(m) {
    1 - sm_area(Y_cd(m)) / sm_area(ref_sf(m), order = ref_id(Y_cd(m)))
}

US3 <- function(m) {
    1 - sm_area(Y_cd(m)) / sm_area(seg_sf(m), order = seg_id(Y_cd(m)))
}

AFI <- function(m) {
    norm_left(sm_area(ref_sf(m), order = ref_id(Y_prime(m))),
              sm_area(seg_sf(m), order = seg_id(Y_prime(m))))
}

QR <- function(m) {
    norm_frac(sm_area(sm_ystar(m)), 
              sm_area(sm_union(x = ref_sf(m), 
                               y = seg_sf(m), 
                               order = Y_star(m))))
}

D_index <- function(m) {
    sqrt((
        norm_frac(sm_area(Y_star(m)),
                  sm_area(ref_sf(m), order = ref_id(Y_star(m)))) ^ 2 +
            norm_frac(sm_area(Y_star(m)),
                      sm_area(seg_sf(m), order = seg_id(Y_star(m)))) ^ 2) / 2)
}

precision <- function(m) {
    sum(sm_area(X_prime(m))) / sum(sm_area(seg_sf(m), order = seg_id(X_prime(m))))
}

recall <- function(m) {
    sum(sm_area(Y_prime(m))) / sum(sm_area(ref_sf(m), order = ref_id(Y_prime(m))))
}

UMerging <- function(m) {
    norm_left(sm_area(ref_sf(m), order = ref_id(Y_star(m))), sm_area(Y_star(m)))
}

OMerging <- function(m) {
    (sm_area(seg_sf(m), order = seg_id(Y_star(m))) - sm_area(Y_star(m))) /
        sm_area(ref_sf(m), order = ref_id(Y_star(m)))
    
}

M <- function(m) {
    sm_area(Y_prime(m)) / 
        sqrt(sm_area(ref_sf(m), order = ref_id(Y_prime(m))) *
                 sm_area(seg_sf(m), order = seg_id(Y_prime(m))))
}

# TODO: check formula in Carleer et al. (2005)
E <- function(m) {
    norm_left(sm_area(seg_sf(m), order = seg_id(X_prime(m))),
              sm_area(X_prime(m))) * 100
}

RAsub <- function(m) {
    sm_area(Y_tilde(m)) / sm_area(ref_sf(m), order = ref_id(Y_tilde(m)))
}

RAsuper <- function(m) {
    sm_area(Y_tilde(m)) / sm_area(seg_sf(m), order = seg_id(Y_tilde(m)))
}

# check formula
PI <- function(m) {
    sm_area(Y_tilde(m)) ^ 2 / (
        sm_area(ref_sf(m), order = ref_id(Y_tilde(m))) *
        sm_area(seg_sf(m), order = seg_id(Y_tilde(m)))
    )
}

# check metric name
F1 <- function(m) {
    (sm_area(seg_sf(m), order = seg_id(X_prime(m))) +
         sm_area(ref_sf(m), order = ref_id(X_prime(m))) -
         2 * sm_area(X_prime(m))) /
        sm_area(seg_sf(m), order = seg_id(X_prime(m)))
}

ED3 <- function(m) {
    sqrt(((1 - sm_area(Y_cd(m)) /
               sm_area(ref_sf(m), order = ref_id(Y_cd(m)))) ^ 2 +
              (1 - sm_area(Y_cd(m)) /
                   sm_area(seg_sf(m), order = seg_id(Y_cd(m)))) ^ 2) / 2)
}

F_measure <- function(m, alpha = 0.5) {
    1 / ((alpha / (sum(sm_area(X_prime(m))) / 
                       sum(sm_area(seg_sf(m), order = seg_id(X_prime(m)))))) + 
             ((1 - alpha) / 
                  (sum(sm_area(Y_prime(m))) / 
                       sum(sm_area(ref_sf(m), order = ref_id(Y_prime(m)))))))
}
