## 5 Užduotis.

library(optimx)

##
## Sumodeliuokite didumo n imtį, gautą stebint
## a) eksponentinį a.d.;
## b) veibulo a.d.
##
## Taikydami tikėtinumų santykio, Valdo ir infomatinį
## kriterijus patikrinkite hipotezę, kad skirstinys eksponentinis, esant
## alternatyvai, kad skirstinys Veibulo.

## Laisvės laipsnių skaičius - kiek turime nežinomų dydžių.

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
            hessian = attr(res, "details")[method, "nhatend"][[1]],
            value = res$value
        )

        return(mle)
    }
}


## Tikėtinumų santykio kriterijus
likelihood_ratio_test <- function(observations, h0_list, h1_list, df, confidence) {

    mle_h0 <-  ml_estimator(
        loglik = h0_list$loglik,
        start_f = h0_list$start_f,
        lower = h0_list$lower,
        upper = h0_list$upper
    )(observations)

    mle_h1 <-  ml_estimator(
        loglik = h1_list$loglik,
        start_f = h1_list$start_f,
        lower = h1_list$lower,
        upper = h1_list$upper
    )(observations)

    r <- list(
        test_statistic = -2 * (mle_h0$value - mle_h1$value),
        critical_value = qchisq(1 - confidence, df = df, lower.tail = FALSE)
        )

    r$drop_null_hypothesis <- r$test_statistic > r$critical_value

    return(r)
}


## Informatinis kriterijus
## Nereikia vertinti \hat{\theta}_n !
information_test <- function(observations, h0_list, df, confidence) {

    mle_h0 <-  ml_estimator(
        loglik = h0_list$loglik,
        start_f = h0_list$start_f,
        lower = h0_list$lower,
        upper = h0_list$upper
    )(observations)

    ts <- -1 * t(as.matrix(mle_h0$gradient)) %*% solve(-1 * mle_h0$hessian) %*% as.matrix(mle_h0$gradient)

    r <- list(
        test_statistic = as.numeric(ts),
        critical_value = qchisq(1 - confidence, df = df, lower.tail = FALSE)
    )

    r$drop_null_hypothesis <- r$test_statistic > r$critical_value

    return(r)
}


## Valdo kriterijus
wald_test <- function(observations, theta_0, h0_list, h1_list, df, confidence) {

    mle_h0 <-  ml_estimator(
        loglik = h0_list$loglik,
        start_f = h0_list$start_f,
        lower = h0_list$lower,
        upper = h0_list$upper
    )(observations)

    mle_h1 <-  ml_estimator(
        loglik = h1_list$loglik,
        start_f = h1_list$start_f,
        lower = h1_list$lower,
        upper = h1_list$upper
    )(observations)

    m <- length(theta_0)
    tmp <- as.matrix(tail(mle_h1$estimates, m) - theta_0)
    ts <- -1 * t(tmp) %*% mle_h0$hessian %*% tmp

    r <- list(
        test_statistic = as.numeric(ts),
        critical_value = qchisq(1 - confidence, df = df, lower.tail = FALSE)
    )

    r$drop_null_hypothesis <- r$test_statistic > r$critical_value

    return(r)
}


moment_exponential <- function(observ) {
    return(mean(observ))
}


loglik_exponential <- function(observ) {

    n <- length(observ)
    s <- sum(observ)

    function(theta) {
        lambda <- theta[1]
        sigma <- 1 / lambda

        return(
            -n * log(sigma) - 1 / sigma * s
        )
    }
}


moment_weibull <- function(observ) {
    gamma <- 0.5772156
    log_observ <- log(observ)
    nu_hat <- sqrt(6 * var(log_observ)) / pi
    mu_hat <- mean(log_observ) + gamma * nu_hat

    eta_hat <- exp(mu_hat)
    sigma_hat <- 1 / nu_hat

    return(c(sigma_hat, eta_hat))
}


loglik_weibull <- function(observ) {

    n <- length(observ)
    sum_log <- sum(log(observ))

    function(theta) {
        sigma <- theta[1]
        eta <- theta[2]

        eq0 <- n * log(eta)
        eq1 <- - n * eta * log(sigma)
        eq2 <- - sigma^(-eta) * sum(observ^eta)
        eq3 <- (eta - 1) * sum_log

        return(eq0 + eq1 + eq2 + eq3)
    }
}

## a)

set.seed(314)
n <- 100L
alpha <- 0.05

lambda <- 1 / 5 ## laisvai pasirinktas
observ <- rexp(n, rate = lambda)

## Tikrinsime hipotezę
##
## H_0: Skirstinys eksponentinis, t.y. eta == 1
## H_1: Skirstinys Veibulo, t.y. eta =/= 1
##
## => Laisvės laipsnių skaičius yra 1.
k <- 1

likelihood_ratio_test(
    observations = observ,
    h0_list = list(
        loglik = loglik_exponential,
        start_f = moment_exponential,
        lower = c(1.0e-6),
        upper = c(Inf)
    ),
    h1_list = list(
        loglik = loglik_weibull,
        start_f = moment_weibull,
        lower = c(1.0e-6, 1.0e-6),
        upper = c(Inf, Inf)
    ),
    df = k,
    confidence = 1 - alpha
)
## $test_statistic
## [1] 0.1229155
##
## $critical_value
## [1] 3.841459
##
## $drop_null_hypothesis
## [1] FALSE
## => nėra pagrindo atmesti H_0

information_test(
    observations = observ,
    h0_list = list(
        loglik = loglik_exponential,
        start_f = moment_exponential,
        lower = c(1.0e-6),
        upper = c(Inf)
    ),
    df = k,
    confidence = 1 - alpha
)
## $test_statistic
## [1] -7.970038e-09
##
## $critical_value
## [1] 3.841459
##
## $drop_null_hypothesis
## [1] FALSE
## => nėra pagrindo atmesti H_0

wald_test(
    observations = observ,
    theta_0 = c(1),
    h0_list = list(
        loglik = loglik_exponential,
        start_f = moment_exponential,
        lower = c(1.0e-6),
        upper = c(Inf)
    ),
    h1_list = list(
        loglik = loglik_weibull,
        start_f = moment_weibull,
        lower = c(1.0e-6, 1.0e-6),
        upper = c(Inf, Inf)
    ),
    df = k,
    confidence = 1 - alpha
)
## $test_statistic
## [1] 1.899425
##
## $critical_value
## [1] 3.841459
##
## $drop_null_hypothesis
## [1] FALSE
## => nėra pagrindo atmesti H_0


## b)

sigma <- 1 / lambda ## laisvai pasirinktas
eta <- 6
observ_difficult <- rweibull(n, shape = eta, scale = sigma)


likelihood_ratio_test(
    observations = observ_difficult,
    h0_list = list(
        loglik = loglik_exponential,
        start_f = moment_exponential,
        lower = c(1.0e-6),
        upper = c(Inf)
    ),
    h1_list = list(
        loglik = loglik_weibull,
        start_f = moment_weibull,
        lower = c(1.0e-6, 1.0e-6),
        upper = c(Inf, Inf)
    ),
    df = k,
    confidence = 1 - alpha
)
## $test_statistic
## [1] 239.8517
##
## $critical_value
## [1] 3.841459
##
## $drop_null_hypothesis
## [1] TRUE
## => H_0 atmetame. Eksponentinio skirstinio šie duomenys neturi.


information_test(
    observations = observ_difficult,
    h0_list = list(
        loglik = loglik_exponential,
        start_f = moment_exponential,
        lower = c(1.0e-6),
        upper = c(Inf)
    ),
    df = k,
    confidence = 1 - alpha
)
## $test_statistic
## [1] -4.928604e-09
##
## $critical_value
## [1] 3.841459
##
## $drop_null_hypothesis
## [1] FALSE
## => Nėra pagrindo atmesti H_0. Galime teigti kad duomenys turi eksponentinį skirstinį.


wald_test(
    observations = observ_difficult,
    theta_0 = c(1),
    h0_list = list(
        loglik = loglik_exponential,
        start_f = moment_exponential,
        lower = c(1.0e-6),
        upper = c(Inf)
    ),
    h1_list = list(
        loglik = loglik_weibull,
        start_f = moment_weibull,
        lower = c(1.0e-6, 1.0e-6),
        upper = c(Inf, Inf)
    ),
    df = k,
    confidence = 1 - alpha
)
## $test_statistic
## [1] 47866.48
##
## $critical_value
## [1] 3.841459
##
## $drop_null_hypothesis
## [1] TRUE
## => H_0 atmetame. Eksponentinio skirstinio šie duomenys neturi.
