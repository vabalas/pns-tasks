## 6. Užduotis

library(optimx)
library(fitdistrplus)
library(flexsurv)

## Tegu laikas iki įvykio turi loglogistinį skirstinį, t.y. išgyvenimo
## funkcija yra $S(t) = 1 - {(1 +(t/\eta)^{-\nu})}^{-1}$.  Gauti
## tokie įvykio momentai (`duom2.txt`):

observ <- c(
    0.151, 0.182, 0.203, 0.204, 0.222, 0.226, 0.235, 0.239, 0.242,
    0.242, 0.248, 0.250, 0.275, 0.279, 0.282, 0.298, 0.299, 0.303,
    0.309, 0.310, 0.333, 0.339, 0.339, 0.344, 0.349, 0.351, 0.353,
    0.360, 0.360, 0.366, 0.376, 0.385, 0.385, 0.392, 0.397, 0.397,
    0.400, 0.404, 0.405, 0.408, 0.411, 0.414, 0.415, 0.431, 0.431,
    0.440, 0.442, 0.447, 0.451, 0.453, 0.459, 0.459, 0.461, 0.467,
    0.469, 0.469, 0.490, 0.491, 0.492, 0.494, 0.510, 0.526, 0.527,
    0.542, 0.551
)

## Įvertinkite parametrus $\alpha$ ir $\gamma$:
## a) naudodami pasirinktą `R` optimizavimo funkciją;

## Pradiniams artiniams vertinti pasinaudosime sąryšiu $T ~ LL(eta, nu)$,
## $Y = log(T) ~ L(mu, sigma^2)$.
##
## Pirmiausia reparametrizuokime loglogistinį skirstinį tokiu būdu:
##
## alpha = 1 / eta^nu, nu = gamma.
##
## Toliau vertinkime logistinio skirstinio parametrus momentu metodu:

mm_logistic <- function(observ) {
    mu_hat <- mean(observ)
    sigma_hat <- sqrt(3 * var(observ) / pi^2)

    return(c(mu_hat, sigma_hat))
}

## Transformavę loglogistinį a. d. į logistinį, įvertinkime mu ir
## sigma, iš kurių po to suskaičiuosime eta ir nu:

mm_loglogistic <- function(observ) {
    logistic_observ <- log(observ)
    theta_logistic_hat <- mm_logistic(logistic_observ)

    mu_hat <- theta_logistic_hat[1]
    sigma_hat <- theta_logistic_hat[2]

    nu_hat <- 1 / sigma_hat
    eta_hat <- exp(mu_hat)

    return(c(eta_hat, nu_hat))
}

## Toliau, momentu metodu paėmę startines reikšmes, DT metodu
## vertiname nu ir eta:

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

loglik <- function(observ) {

    n <- length(observ)
    log_sum <- sum(log(observ))

    function(theta) {
        eta <- theta[1]
        nu <- theta[2]

        eq1 <- n * (log(nu) - nu * log(eta))
        eq2 <- (nu - 1) * log_sum
        eq3 <- 2 * sum(log(1 + (observ / eta)^nu))

        return(eq1 + eq2 - eq3)
    }
}

(mm_loglogistic(observ))
##          eta      nu
## > [1] 0.3575598 6.1335150

loglogistic_mle <- ml_estimator(
    loglik = loglik,
    start_f = mm_loglogistic,
    lower = c(1.0e-6, 1.0e-6),
    upper = c(Inf, Inf)
)(observ)

(loglogistic_mle$estimates)
## >         eta      nu
## > [1] 0.3680722 5.9667936

## Liko tik transformuoti atgal į alpha ir gamma:
eta_hat <- loglogistic_mle$estimates[1]
nu_hat <- loglogistic_mle$estimates[2]

(alpha_hat <- 1 / eta_hat^nu_hat)
## > [1] 389.0346
(gamma_hat <- nu_hat)
## > [1] 5.966794


## b) funkciją `fitdistr()`.

fd <- fitdistrplus::fitdist(observ, "llogis", method = "mle")

(nu_hat_fd <- unname(fd$estimate[1]))
## > [1] 5.968047
(eta_hat_fd <- unname(fd$estimate[2]))
## > [1] 0.3680626

(alpha_hat_fd <- 1 / eta_hat_fd^nu_hat_fd)
## > [1] 389.583
(gamma_hat_fd <- nu_hat_fd)
## > [1] 5.968047


## Raskite medianą, 70-ąjį procentilį.
quantile <- function(alpha, gamma) {
    function(q) {
        (q / (alpha * (1 - q)))^(1 / gamma)
    }
}

(quantile(alpha_hat, gamma_hat)(0.5))
## > [1] 0.3680722
(quantile(alpha_hat, gamma_hat)(0.7))
## > [1] 0.4242323
