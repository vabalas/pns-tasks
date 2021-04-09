## 3. Užduotis

set.seed(314)

## Modeliuokite a. d. $X$, turinčio binominį skirstinį su parametrais
## $k$ ir $p$, t. y.
##
## $$
## P(X = m | k, p) = C^m_k  p^m (1 − p)^{k−m},
## $$
##
## dydžio $n = 50$ paprastąją atsitiktinę imtį. Naudodami `R`
## programos funkciją `maxLik` arba `optim` DT metodu įvertinkite
## parametrą $p$. Palyginkite su tikra parametro reikšme.

n <- 50L
(p <- 1 / pi)
## > [1] 0.3183099
k <- 100L
observ <- rbinom(n, k, p)


## Momentų metodas
p_hat_moment <- function(observ) {
    return(1 - var(observ) / mean(observ))
}

## k_hat_moment <- function(observ) {
##     m <- mean(observ)
##     return(m^2 / (m - var(observ)))
## }


binom_loglik_p <- function(observ) {

    ## Precalculate:
    n <- length(observ)
    m <- mean(observ)
    tmp1 <- n * m
    tmp2 <- n * (k - m)
    tmp3 <- sum(lfactorial(k) - lfactorial(observ) - lfactorial(k - observ))

    function(theta) {
        p <- theta[1]
        return(
            tmp1 * log(p)
            + tmp2 * log(1 - p)
            + tmp3
        )
    }
}


ml_estimator <- function(loglik, start_f, method = "L-BFGS-B", ...) {
    require(optimx)
    function(observations) {

        start <- start_f(observations)

        res <- optimx(
            par = start,
            fn = loglik(observations),
            control = list(
                maximize = TRUE
            ),
            method = method,
            hessian = TRUE,
            ...
        )

        mle <- list(
            estimates = as.numeric(res[, paste0("p", 1:attr(res, "npar"))]),
            gradient = attr(res, "details")[method, "ngatend"][[1]],
            hessian = attr(res, "details")[method, "nhatend"][[1]]
        )

        return(mle)
    }
}

binom_p_mle <- ml_estimator(
    loglik = binom_loglik_p,
    start_f = p_hat_moment,
    lower = c(1.0e-6),
    upper = c(1 - 1.0e-6)
)(observ)

## DT įvertis:
(binom_p_mle$estimates)
## > [1] 0.3238005

## Momentų metodo įvertis:
(p_hat_moment(observ))
## > [1] 0.4833167

## Tikroji p reikšmė:
## > [1] 0.3183099
