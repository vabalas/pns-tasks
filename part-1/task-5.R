## 5. Užduotis

set.seed(314)
library(MASS)

## Duomenys iš 1) ir 2). Įvertinkite parametrus naudodami `R` funkciją
## `fitdistr(duomenys, "skirstinys")`

n <- 100L

lambda <- 7 ## laisvai pasirinkta reikšmė

exp_inverse_cdf <- function(theta) { ## Eksponentinis
    function(probs) {
        -1 / theta[1] * log(1 - probs)
    }
}

exp_observ <- exp_inverse_cdf(lambda)(runif(n))

eta <- 2 # laisvai pasirinkti
nu <- 2

weibull_inverse_cdf <- function(theta) {
    function(probs) {
        theta[1] * (-1 * log(1 - probs)) ^ (1 / theta[2])
    }
}

weibull_observ <- weibull_inverse_cdf(c(eta, nu))(runif(n))

fitdistr(weibull_observ, "Weibull")
## >    shape       scale
## >  2.0686329   2.2249776
## > (0.1632889) (0.1131470)

fitdistr(exp_observ, "exponential")
## >     rate
## >  8.8168220
## > (0.8816822)
