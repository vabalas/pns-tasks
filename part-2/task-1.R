## 1. Užduotis

set.seed(314)

library(latex2exp)

## Duomenys iš pirmos užduoties 2) punkto, t.y. stebėtas a.d.,
## turintis Veibulo skirstinį su parametrais $\eta$ ir $\nu$,
## t. y. a. d. $T$ pasiskirstymo funkcija yra
##
## $$
## F(t; \eta, \nu) = 1 - \exp{\{ - {(t / \eta)}^\nu \}},
## $$
##
## gauta didumo $n = 100$ paprastoji atsitiktinė imtis.

n <- 100L
## Laisvai pasirinktos parametrų reikšmės:
eta <- 2
nu <- 2

weibull_inverse_cdf <- function(theta) {
    function(probs) {
        theta[1] * (-1 * log(1 - probs)) ^ (1 / theta[2])
    }
}

observ <- weibull_inverse_cdf(c(eta, nu))(runif(n))

## Taškiniai įverčiai buvo rasti pirmoje užduotyje.
## Analogiškai pirmosios dalies antrai užduočiai:

gamma <- 0.5772156
sigma_hat_moment <- function(x) { sqrt(6 * var(x)) / pi }
mu_hat_moment <- function(x) { mean(x) + gamma * sigma_hat_moment(x) }

(start_eta <- exp(mu_hat_moment(log(observ))))
## > [1] 1.820739
(start_nu <- 1 / sigma_hat_moment(log(observ)))
## > [1] 1.919287

weibull_loglik <- function(observ) {
    log_sum <- sum(log(observ))
    n <- length(observ)

    function(theta) {
        eta <- theta[1]
        nu <- theta[2]

        eq1 <- n * (log(eta) - log(nu))
        eq2 <- - (1 / nu^eta) * sum(observ^eta)
        eq3 <- (eta - 1) * (log_sum - n * log(nu))

        return(eq1 + eq2 + eq3)
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

weibull_mle <- ml_estimator(
    loglik = weibull_loglik,
    start_f = function(x) { c(start_eta, start_nu) },
    lower = c(1.0e-6, 1.0e-6),
    upper = c(Inf, Inf)
)(observ)

## a) Raskite stebėtą Fišerio informacinę matricą.

(fishers_info <- -1 * weibull_mle$hessian)
##           [,1]      [,2]
## [1,]  41.64965 -22.71778
## [2,] -22.71778 133.21490


## b) Raskite parametrų pasikliovimo intervalus. Rezultatus (tikros
##    parametrų reikšmės, taškiniai įverčiai ir pasikliovimo intervalai)
##    pateikite R duomenų lentelėje (dataframe).

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

alpha <- 0.03
ci <- mle_asymptotic_ci(weibull_mle, n)(1 - alpha)
eta_hat_mle <- weibull_mle$estimates[1]
nu_hat_mle <- weibull_mle$estimates[2]

data.frame(
    name = c("eta", "nu"),
    actual = c(eta, nu),
    lo = c(ci[[1]]$lo, ci[[2]]$lo),
    ml_estimate = c(eta_hat_mle, nu_hat_mle),
    hi = c(ci[[1]]$hi, ci[[2]]$hi)
)

##   name actual       lo ml_estimate       hi
## 1  eta      2 2.035542    2.070850 2.106158
## 2   nu      2 1.758897    1.794205 1.829513


## c) Raskite tikimybės, kad stebėtas a.d. įgis reikšmę didesnę už t,
##    pasikliovimo intervalą. Rezultatus (t, tikrą tikimybės reikšmę,
##    tikimybės įvertį, pasikliovimo lygmenį, pasikliovimo intervalą)
##    pateikite R duomenų lentelėje (dataframe).

## Tikimybė kad $X$ reikšmė didesnė už $t$ arba $P(X > t) = 1 - F(x) =
## S(x)$, kitaip tariant - išgyvenamumo funkcija. Taigi, turime rasti
## $S(t)$ įverčio pasikliovimo intervalą.

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

### Išgyvenamumo funkcijos S(t; theta) įverčio asimptotinių
### pasikliovimo intervalų aproksimacija.
mle_asymptotic_ci_survival <- function(mle, survival_f, survival_partials, confidence, sample_size) {

    alpha <- 1 - confidence
    critical_value <- qnorm(alpha, mean = 0, sd = 1, lower.tail = FALSE)
    variance_survival <- variance_effective_estimator(mle, sample_size, survival_partials)
    survival_hat <- survival_f(mle$estimates)
    sqrt_n <- sqrt(sample_size)

    ### Išgyvenamumo funkcijos įverčios asimptotinis pasikliovimo intervalas taške t.
    function(t) {
        variance_at_t <- variance_survival(t)
        survival_at_t <- survival_hat(t)

        expr0 <- sqrt(variance_at_t) / (survival_at_t * (1 - survival_at_t))
        expr1 <- (1 - survival_at_t) / survival_at_t
        expr2 <- expr0 * critical_value / sqrt_n

        lo <- 1 / (1 + expr1 * exp(expr2))
        hi <- 1 / (1 + expr1 * exp(-expr2))

        return(c(lo = lo, mid = survival_at_t, hi = hi))
    }
}

survival <- function(theta) {
    eta <- theta[1]
    nu <- theta[2]

    function(t) {
        exp(-1 * (t / eta)^nu)
    }
}

diff_survival_d_eta <- function(theta) {
    eta <- theta[1]
    nu <- theta[2]

    function(t) {
         nu * t * (t / eta)^(nu - 1) * exp(-(t / eta)^nu) / eta^2
    }
}

diff_survival_d_nu <- function(theta) {
    eta <- theta[1]
    nu <- theta[2]

    function(t) {
        -(t / eta)^nu * exp(-(t / eta)^nu) * log(t / eta)
    }
}

weibull_survival_ci <- mle_asymptotic_ci_survival(
    mle = weibull_mle,
    survival_f = survival,
    survival_partials = c(diff_survival_d_eta, diff_survival_d_nu),
    confidence = 1 - alpha,
    sample_size = n
)

weibull_qs <- seq(0, 10, length.out = 100)

weibull_survival_df <- data.frame(t(sapply(weibull_qs, function(t) {
    estim <- unname(weibull_survival_ci(t))
    actual <- survival(c(eta, nu))(t)

    r <- c(
        t = t,
        actual = actual,
        lo = estim[1],
        mid = estim[2],
        hi = estim[3]
    )

    return(r)
})))

plot_ci_survival <- function(df, main, ...) {
    plot(df$t, df$actual, col = "black", type = "l", lwd = 3,
        xlab = "", ylab = "", ...)

    lines(df$t, df$mid, col = "black", lwd = 3, lty = 2)
    lines(df$t, df$lo, col = "blue", lwd = 2, lty = 1)
    lines(df$t, df$hi, col = "red", lwd = 2, lty = 1)

    title(main = main, xlab = "t", ylab = "S(t)")

    legend(
        "topright",
        legend = c(
            latex2exp::TeX("$S(t)$"),
            latex2exp::TeX("$\\hat{S}(t)$"),
            latex2exp::TeX("upper bound $\\hat{S}(t)$"),
            latex2exp::TeX("lower bound $\\hat{S}(t)$")
        ),
        col = c("black", "black", "red", "blue"),
        lty = c(1, 2, 1, 1),
        lwd = c(3, 3, 2, 2)
    )
}

plot_ci_survival(
    weibull_survival_df,
    sprintf("%d%% CI for the estimate of the Weibull survival function", (1 - alpha) * 100)
)

## d) Raskite intervalinį medianos įvertį. Rezultatus (tikrą medianos
##    reikšmę, taškinį įvertį, pasikliovimo lygmenį ir pasikliovimo
##    intervalą) pateikite duomenų lentelėje (dataframe).

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

quantile <- function(theta) {
    eta <- theta[1]
    nu <- theta[2]

    function(q) {
        return(eta * (-1 * log(1 - q)) ^ (1 / nu))
    }
}

diff_quantile_eta <- function(theta) {
    eta <- theta[1]
    nu <- theta[2]

    function(q) {
        (-log(-q + 1))^(1 / nu)
    }
}

diff_quantile_nu <- function(theta) {
    eta <- theta[1]
    nu <- theta[2]

    function(q) {
        -eta * (-log(-q + 1))^(1 / nu) * log(-log(-q + 1)) / nu^2
    }
}

weibull_quantile_ci <- mle_asymptotic_ci_generic(
    mle = weibull_mle,
    param_f = quantile,
    partials = c(diff_quantile_eta, diff_quantile_nu),
    confidence = 1 - alpha,
    sample_size = n
)

weibull_ps <- sort(c(seq(0.1, .99, length.out = 100), 0.5))
weibull_quantile_df <- data.frame(t(sapply(weibull_ps, function(k) {
    estim <- unname(weibull_quantile_ci(k))
    actual <- quantile(c(eta, nu))(k)

    r <- c(
        q = k,
        actual = actual,
        lo = estim[1],
        mid = estim[2],
        hi = estim[3]
    )

    return(r)
})))


(weibull_quantile_df[weibull_quantile_df$q == 0.5, ])
##      q   actual       lo      mid      hi
## 46 0.5 1.665109 1.426849 1.688235 1.94926

plot_ci_quantile <- function(df, main, ...) {
    plot(df$q, df$actual, col = "black", type = "l", lwd = 3,
        xlab = "", ylab = "", ...)

    lines(df$q, df$mid, col = "black", lwd = 3, lty = 2)
    lines(df$q, df$lo, col = "blue", lwd = 2, lty = 1)
    lines(df$q, df$hi, col = "red", lwd = 2, lty = 1)

    title(
        main = main,
        xlab = "q",
        ylab = latex2exp::TeX("$x_q \\equiv x: F(x) == q$")
    )

    legend(
        "topleft",
        legend = c(
            latex2exp::TeX("$x_q$"),
            latex2exp::TeX("$\\hat{x_q}$"),
            latex2exp::TeX("upper bound $\\hat{x_q}$"),
            latex2exp::TeX("lower bound $\\hat{x_q}$")
        ),
        col = c("black", "black", "red", "blue"),
        lty = c(1, 2, 1, 1),
        lwd = c(3, 3, 2, 2)
    )
}

plot_ci_quantile(
    weibull_quantile_df,
    sprintf("%d%% CI for the estimate of the Weibull quantile function", (1 - alpha) * 100)
)


## e) Raskite p-ojo kvantilio pasikliovimo intervalą. Rezultatus
##    (kvantilio lygmenį p, tikrą kvantilio reikšmę, taškinį įvertį,
##    pasikliovimo lygmenį ir pasikliovimo intervalą) pateikite R duomenų
##    lentelėje (dataframe).

(weibull_quantile_df[weibull_quantile_df$q == weibull_ps[10], ])
##            q    actual       lo      mid       hi
## 10 0.1809091 0.8934432 0.681416 0.843422 1.005428
