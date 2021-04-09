## 3. Užduotis

set.seed(314)

## Duomenys iš pirmos užduoties 3) punkto, t.y. a.d. $X$, turinčio
## binominį skirstinį su parametrais $k$ ir $p$, t. y.
##
## $$
## P(X = m | k, p) = C^k_m p^m {(1 − p)}^{k−m},
## $$
##
## dydžio $n = 50$ paprastoji atsitiktinė imtis.
##
## Raskite tikslų parametro p pasikliovimo intervalą naudodami beta
## skirstinio kvantilius.

n <- 50L
## Laisvai pasirinkime teorinius parametrus:
k <- 20
p <- 0.4

observ <- rbinom(n, k, p)

## Didžiausio tikėtinumo metodu rasime parametru įverčius:

loglik <- function(observations) {
    function(theta) {
        p <- theta[1]

        sum(
            log(choose(k, observations))
            + log(p) * observations
            + log(1 - p) * (k - observations)
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

moment_estimator <- function(observations) {
    mean(observations) / length(observations)
}

(start_theta <- moment_estimator(observ))
## > [1] 0.1464

binom_mle <- ml_estimator(
    loglik = loglik,
    start_f = moment_estimator,
    lower = c(1.0e-6),
    upper = c(1 - 1.0e-6)
)(observ)

(binom_mle$estimates)
## > [1] 0.3660004

## Toliau remsimės pavyzdžiu iš vadovėlio 141-142 puslapio:
confidence <- 0.95
alpha <- 1 - confidence
(alpha / 2)
## > [1] 0.025

complete_statistic <- sum(observ)
p_lo <- 1 - qbeta(1 - alpha / 2, n * k - complete_statistic + 1, complete_statistic)
p_hi <- 1 - qbeta(alpha / 2, n * k - complete_statistic, complete_statistic + 1)
p_hat <- binom_mle$estimates[1]

data.frame(
    name = "p",
    actual = p,
    low = p_lo,
    estimate = p_hat,
    high = p_hi
)
##   name actual       low  estimate      high
## 1    p    0.4 0.3360757 0.3660004 0.3967159
