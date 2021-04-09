## 6. Užduotis

library(optimx)
library(MASS) # fitdistr()

## Tegu laikas iki įvykio turi loglogistinį skirstinį, t.y. išgyvenimo
## funkcija yra $S(t) = 1 - {(1 +(t/\eta)^{-\nu})}^{-1}$.  Gauti
## tokie įvykio momentai (`duom2.txt`):

observ_survival_moments <- c(
    0.151, 0.182, 0.203, 0.204, 0.222, 0.226, 0.235, 0.239, 0.242,
    0.242, 0.248, 0.250, 0.275, 0.279, 0.282, 0.298, 0.299, 0.303,
    0.309, 0.310, 0.333, 0.339, 0.339, 0.344, 0.349, 0.351, 0.353,
    0.360, 0.360, 0.366, 0.376, 0.385, 0.385, 0.392, 0.397, 0.397,
    0.400, 0.404, 0.405, 0.408, 0.411, 0.414, 0.415, 0.431, 0.431,
    0.440, 0.442, 0.447, 0.451, 0.453, 0.459, 0.459, 0.461, 0.467,
    0.469, 0.469, 0.49, 0.491, 0.492, 0.494, 0.510, 0.526, 0.527,
    0.542, 0.551
)

## Įvertinkite parametrus $\alpha$ ir $\gamma$:
## a) naudodami pasirinktą `R` optimizavimo funkciją;

loglogistic_loglik <- function(observ) {

    n <- length(observ)
    log_sum <- sum(log(observ))

    function(theta) {
        alpha <- theta[1]
        gamma <- theta[2]
        print(alpha)
        print(gamma)
        eq1 <- n * (log(alpha) + log(gamma))
        eq2 <- (gamma - 1) * log_sum
        eq3 <- 2 * sum(1 - alpha * observ^gamma)

        return(eq1 + eq2 - eq3)
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

loglogistic_mle <- ml_estimator(
    loglik = loglogistic_loglik,
    start_f = function(x) { c(1, 1) }, ## Kaip parinkti startines reikšmes?
    lower = c(1.0e-6, 1.0e-6),
    upper = c(Inf, Inf)
)(observ)

(loglogistic_mle$estimates)
## > [1] 32.17391 13.66364

## Galime pastebėti, kad išbandžius skirtingas startines reikšmes,
## rezultas "smarkiai" nesiskiria, todėl laikykime kad toks DT įvertis
## yra patenkinamas.

## b) funkciją `fitdistr`.
## ???

## Raskite medianą, 70-ąjį kvartilį.

