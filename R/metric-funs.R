
OS1 <- function(m) {
    sm_norm_frac(sm_area(sm_ystar(m)), sm_area(sm_ref(m)))
}

OS1 <- function(m) {
    norm_frac(sm_area(sm_ystar(m)), sm_area(sm_ref(m), order = sm_ystar(m)))
}

US1 <- function(m) {
    norm_frac(sm_area(sm_ystar(m)), sm_area(sm_seg(m), order = sm_ystar(m)))
}

OS2 <- function(m) {
    norm_frac(sm_area(sm_yprime(m)), sm_area(sm_ref(m), order = sm_yprime(m)))
}

US2 <- function(m) {
    norm_frac(sm_area(sm_yprime(m)), sm_area(sm_seg(m), order = sm_yprime(m)))
}

OS3 <- function(m) {
    1 - sm_area(sm_ycd(m)) / sm_area(sm_ref(m), order = sm_ycd(m))
}

US3 <- function(m) {
    1 - sm_area(sm_ycd(m)) / sm_area(sm_seg(m), order = sm_ycd(m))
}

AFI <- function(m) {
    norm_left(sm_area(sm_ref(m), order = sm_yprime(m)),
              sm_area(sm_seg(m), order = sm_yprime(m)))
}

QR <- function(m) {
    norm_frac(sm_area(sm_ystar(m)), 
              sm_area(sm_union(x = sm_ref(m), 
                               y = sm_seg(m), 
                               order = sm_ystar(m))))
}

D_index <- function(m) {
    sqrt((
        norm_frac(sm_area(sm_ystar(m)),
                  sm_area(sm_ref(m), order = sm_ystar(m))) ^ 2 +
            norm_frac(sm_area(sm_ystar(m)),
                      sm_area(sm_seg(m), order = sm_ystar(m))) ^ 2) / 2)
}

precision <- function(m) {
    sum(sm_area(sm_xprime(m))) / sum(sm_area(sm_seg(m), order = sm_xprime(m)))
}

recall <- function(m) {
    sum(sm_area(sm_yprime(m))) / sum(sm_area(sm_ref(m), order = sm_yprime(m)))
}

UMerging <- function(m) {
    norm_left(sm_area(sm_ref(m), order = sm_ystar(m)), sm_area(sm_ystar(m)))
}

OMerging <- function(m) {
    (sm_area(sm_seg(m), order = sm_ystar(m)) - sm_area(sm_ystar(m))) /
        sm_area(sm_ref(m), order = sm_ystar(m))
    
}

M <- function(m) {
    sm_area(sm_yprime(m)) / 
        sqrt(sm_area(sm_ref(m), order = sm_yprime(m)) *
                 sm_area(sm_seg(m), order = sm_yprime(m)))
}

# TODO: check formula in Carleer et al. (2005)
E <- function(m) {
    norm_left(sm_area(sm_seg(m), order = sm_xprime(m)),
              sm_area(sm_xprime(m))) * 100
}

RAsub <- function(m) {
    sm_area(sm_ytilde(m)) / sm_area(sm_ref(m), order = sm_ytilde(m))
}

RAsuper <- function(m) {
    sm_area(sm_ytilde(m)) / sm_area(sm_seg(m), order = sm_ytilde(m))
}

# check formula
PI <- function(m) {
    sm_area(sm_ytilde(m)) ^ 2 / (
        sm_area(sm_ref(m), order = sm_ytilde(m)) *
        sm_area(sm_seg(m), order = sm_ytilde(m))
    )
}

# check metric name
F1 <- function(m) {
    (sm_area(sm_seg(m), order = sm_xprime(m)) +
         sm_area(sm_ref(m), order = sm_xprime(m)) -
         2 * sm_area(sm_xprime(m))) /
        sm_area(sm_seg(m), order = sm_xprime(m))
}

ED3 <- function(m) {
    sqrt(((1 - sm_area(sm_ycd(m)) /
               sm_area(sm_ref(m), order = sm_ycd(m))) ^ 2 +
              (1 - sm_area(sm_ycd(m)) /
                   sm_area(sm_seg(m), order = sm_ycd(m))) ^ 2) / 2)
}

F_measure <- function(m, alpha = 0.5) {
    1 / ((alpha / (sum(sm_area(sm_xprime(m))) / 
                       sum(sm_area(sm_seg(m), order = sm_xprime(m))))) + 
             ((1 - alpha) / 
                  (sum(sm_area(sm_yprime(m))) / 
                       sum(sm_area(sm_ref(m), order = sm_yprime(m))))))
}
