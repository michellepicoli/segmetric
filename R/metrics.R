
OS1 <- function(m) {
    norm_frac(area(Y_star(m)), area(ref_sf(m), order = ref_id(Y_star(m))))
}

US1 <- function(m) {
    norm_frac(area(Y_star(m)), area(seg_sf(m), order = seg_id(Y_star(m))))
}

OS2 <- function(m) {
    norm_frac(area(Y_prime(m)), area(ref_sf(m), order = ref_id(Y_prime(m))))
}

US2 <- function(m) {
    norm_frac(area(Y_prime(m)), area(seg_sf(m), order = seg_id(Y_prime(m))))
}

OS3 <- function(m) {
    1 - area(Y_cd(m)) / area(ref_sf(m), order = ref_id(Y_cd(m)))
}

US3 <- function(m) {
    1 - area(Y_cd(m)) / area(seg_sf(m), order = seg_id(Y_cd(m)))
}

AFI <- function(m) {
    norm_left(area(ref_sf(m), order = ref_id(Y_prime(m))),
              area(seg_sf(m), order = seg_id(Y_prime(m))))
}

QR <- function(m) {
    norm_frac(area(Y_star(m)), area(union2(Y_star(m), seg_sf(m), ref_sf(m))))
}

D_index <- function(m) {
    sqrt((
        norm_frac(area(Y_star(m)),
                  area(ref_sf(m), order = ref_id(Y_star(m)))) ^ 2 +
            norm_frac(area(Y_star(m)),
                      area(seg_sf(m), order = seg_id(Y_star(m)))) ^ 2) / 2)
}

precision <- function(m) {
    sum(area(X_prime(m))) / sum(area(seg_sf(m), order = seg_id(X_prime(m))))
}

recall <- function(m) {
    sum(area(Y_prime(m))) / sum(area(ref_sf(m), order = ref_id(Y_prime(m))))
}

UMerging <- function(m) {
    norm_left(area(ref_sf(m), order = ref_id(Y_star(m))), area(Y_star(m)))
}

OMerging <- function(m) {
    (area(seg_sf(m), order = seg_id(Y_star(m))) - area(Y_star(m))) /
        area(ref_sf(m), order = ref_id(Y_star(m)))
    
}

M <- function(m) {
    area(Y_prime(m)) / 
        sqrt(area(ref_sf(m), order = ref_id(Y_prime(m))) *
                 area(seg_sf(m), order = seg_id(Y_prime(m))))
}

# TODO: check formula in Carleer et al. (2005)
E <- function(m) {
    norm_left(area(seg_sf(m), order = seg_id(X_prime(m))),
              area(X_prime(m))) * 100
}

RAsub <- function(m) {
    area(Y_tilde(m)) / area(ref_sf(m), order = ref_id(Y_tilde(m)))
}

RAsuper <- function(m) {
    area(Y_tilde(m)) / area(seg_sf(m), order = seg_id(Y_tilde(m)))
}

# check formula
PI <- function(m) {
    area(Y_tilde(m)) ^ 2 /
        area(ref_sf(m), order = ref_id(Y_tilde(m))) /
        area(seg_sf(m), order = ref_id(Y_tilde(m)))
}

# check metric name
F1 <- function(m) {
    (area(seg_sf(m), order = seg_id(X_prime(m))) +
         area(ref_sf(m), order = ref_id(X_prime(m))) -
         2 * area(X_prime(m))) /
        area(seg_sf(m), order = seg_id(X_prime(m)))
}

ED3 <- function(m) {
    sqrt(((1 - area(Y_cd(m)) /
               area(ref_sf(m), order = ref_id(Y_cd(m)))) ^ 2 +
              (1 - area(Y_cd(m)) /
                   area(seg_sf(m), order = seg_id(Y_cd(m)))) ^ 2) / 2)
}

F_measure <- function(m, alpha = 0.5) {
    1 / ((alpha / (sum(area(X_prime(m))) / 
                       sum(area(seg_sf(m), order = seg_id(X_prime(m)))))) + 
             ((1 - alpha) / 
                  (sum(area(Y_prime(m))) / 
                       sum(area(ref_sf(m), order = ref_id(Y_prime(m)))))))
}
