## 2. Užduotis

set.seed(314)

library(latex2exp)


## Tarkime, kad laikas iki įvykio $T$ turi ekstremalių reikšmių
## (minimalių) skirstinį, t.y.  pasiskirstymo funkcija
##
## $$
## F(t; \mu, \sigma) = 1 - \exp{\{-{e}^{(t - \mu)/\sigma}\}},
## $$
##
## Naudodami atvirkštinės transformacijos metodą, sumodeliuokite
## paprastąją atsitiktinę imtį (Pastaba. Kai iš konteksto aišku,
## dažnai žodis realizacija praleidžiamas).

inverse_cdf <- function(theta) {
    mu <- theta[1]
    sigma <- theta[2]

    function(t) {
        sigma * log(-log(1 - t)) + mu
    }
}

n <- 100L
alpha <- 0.03

## Laisvai pasirikti teoriniai parametrai
mu <- 5
sigma <- 10

observ <- inverse_cdf(c(mu, sigma))(runif(n))



## Raskite taškinius ir intervalinius parametrų įverčius.

## Logtikėtinumo funkcija
loglik <- function(observations) {
    function(theta) {
        mu <- theta[1]
        sigma <- theta[2]

        sum(-log(sigma) + (observations - mu) / sigma - exp((observations - mu) / sigma))
    }
}

## Pradiniams artiniams ieškoti naudosimės momentų metodu (parametrą
## perašysime kaip funkciją nuo vidurkio ir įstatę imties vidurkį
## gausime įvertį):
##
## $$
## \hat{\mu} = \frac{\pi \bar{x} + \sqrt{6} \gamma \sqrt{s^2}}{\pi},
## \hat{\sigma} = \frac{\sqrt{6} \sqrt{s^2}}{\pi}
## $$
##
## čia $\bar{x}$ - imties vidurkis, $s^2$ - imties variacija.

moment_estimator <- function(observations) {
    gamma <- 0.5772156

    m <- mean(observations)
    s <- var(observations)

    sigma_hat <- sqrt(6) * sqrt(s) / pi
    mu_hat <- m + gamma * sigma_hat

    return(c(mu_hat, sigma_hat))
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

(start_theta <- moment_estimator(observ))
## > [1] 7.135234 9.826371

extreme_mle <- ml_estimator(
    loglik = loglik,
    start_f = moment_estimator,
    lower = c(-Inf, 1.0e-6),
    upper = c(Inf, Inf)
)(observ)

(extreme_mle$estimates)
## > [1] 7.131968 9.668215

## Pasinaudosime didžiausio tikėtinumo įvertinių asimptotiniu
## normalumu:

### Asimptotinis DT įverčio pasikliovimo intervalas
mle_asymptotic_ci <- function(mle, sample_size) {
    mle_estimates <- mle$estimates
    fishers_info <- -1 * mle$hessian

    ## Skaičiuosime tik vieną kartą:
    fishers_inverse <- solve(fishers_info)
    sqrt_size <- sqrt(sample_size)

    function(confidence) {
        i <- 1
        alpha_half <- (1 - confidence) / 2

        lapply(mle_estimates, function(e) {
            variance <- fishers_inverse[i, i]
            shift <- sqrt(variance) * qnorm(alpha_half, lower.tail = FALSE) / sqrt_size
            i <- i + 1

            ci <- list(
                lo = e - shift,
                mid = e,
                hi = e + shift
            )

            return(ci)
        })
    }
}

ci <- mle_asymptotic_ci(extreme_mle, n)(1 - alpha)
mu_hat_mle <- extreme_mle$estimates[1]
sigma_hat_mle <- extreme_mle$estimates[2]

data.frame(
    name = c("mu", "sigma"),
    actual = c(mu, sigma),
    lo = c(ci[[1]]$lo, ci[[2]]$lo),
    ml_estimate = c(mu_hat_mle, sigma_hat_mle),
    hi = c(ci[[1]]$hi, ci[[2]]$hi)
)
##    name actual       lo ml_estimate       hi
## 1    mu      5 6.911257    7.131968 7.352680
## 2 sigma     10 9.447503    9.668215 9.888926



## Pasirinktame taške t įvertinkite pasiskirstymo funkciją ir jos
## pasikliovimo intervalą.

### Pagrįstasis parametrų funkcijos variacijos įvertinys.
variance_effective_estimator <- function(mle, sample_size, partials) {
    inverse_hessian <- solve(mle$hessian)

    ## Įstatykime parametrų įverčius į dalines išgyvenamumo funkcijos išvestines
    partials <- sapply(partials, function(p) { p(mle$estimates) })

    ### Parametrų funkcijos variacija taške t
    function(t) {
        ## Suskaičiuokime dalinių išvestinių reikšmes taške t
        partials_at_t <- sapply(partials, function(p) { p(t) })
        return(as.numeric(-sample_size * (t(partials_at_t) %*% inverse_hessian %*% partials_at_t)))
    }
}

### Bendra parametrų ir vieno argumento funkcijos g(t; theta)
### asimptotinio pasikliovimo intervalo funkcija.
mle_asymptotic_ci_generic <- function(mle, param_f, partials, confidence, sample_size) {

    alpha <- 1 - confidence
    critical_value <- qnorm(alpha, mean = 0, sd = 1, lower.tail = FALSE)
    param_f_variance <- variance_effective_estimator(mle, sample_size, partials)
    f <- param_f(mle$estimates)
    sqrt_n <- sqrt(sample_size)

    ### Parametrų funkcijos g(t; theta) pasikliovimo intervalas taške t.
    function(t) {
        f_at_t <- f(t)
        shift <- sqrt(param_f_variance(t)) * critical_value / sqrt_n
        lo <- f_at_t - shift
        hi <- f_at_t + shift

        return(c(lo = lo, mid = f_at_t, hi = hi))
    }
}

cdf <- function(theta) {
    mu <- theta[1]
    sigma <- theta[2]

    function(t) {
        1 - exp(-exp((t - mu) / sigma))
    }
}

diff_cdf_mu <- function(theta) {
    mu <- theta[1]
    sigma <- theta[2]

    function(t) {
        -exp(-(mu - t) / sigma - exp(-(mu - t) / sigma)) / sigma
    }
}

diff_cdf_sigma <- function(theta) {
    mu <- theta[1]
    sigma <- theta[2]

    function(t) {
        (mu - t) * exp(-(mu - t) / sigma - exp(-(mu - t) / sigma)) / sigma^2
    }
}

extreme_cdf_ci <- mle_asymptotic_ci_generic(
    mle = extreme_mle,
    param_f = cdf,
    partials = c(diff_cdf_mu, diff_cdf_sigma),
    confidence = 1 - alpha,
    sample_size = n
)

extreme_ts <- seq(-10, 10, length.out = 100)
extreme_cdf_df <- data.frame(t(sapply(extreme_ts, function(k) {
    estim <- unname(extreme_cdf_ci(k))
    actual <- cdf(c(mu, sigma))(k)

    r <- c(
        t = k,
        actual = actual,
        lo = estim[1],
        mid = estim[2],
        hi = estim[3]
    )

    return(r)
})))

plot_ci_cdf <- function(df, main, ...) {
    plot(df$t, df$actual, col = "black", type = "l", lwd = 3,
        xlab = "", ylab = "", ...)

    lines(df$t, df$mid, col = "black", lwd = 3, lty = 2)
    lines(df$t, df$lo, col = "blue", lwd = 2, lty = 1)
    lines(df$t, df$hi, col = "red", lwd = 2, lty = 1)

    title(main = main, xlab = "t", ylab = "F(t)")

    legend(
        "topright",
        legend = c(
            latex2exp::TeX("$F(t)$"),
            latex2exp::TeX("$\\hat{F}(t)$"),
            latex2exp::TeX("upper bound $\\hat{F}(t)$"),
            latex2exp::TeX("lower bound $\\hat{F}(t)$")
        ),
        col = c("black", "black", "red", "blue"),
        lty = c(1, 2, 1, 1),
        lwd = c(3, 3, 2, 2)
    )
}

plot_ci_cdf(
    extreme_cdf_df,
    sprintf("%d%% CI for the estimate of the Extreme Value cdf", (1 - alpha) * 100)
)
