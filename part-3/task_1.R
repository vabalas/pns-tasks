## Pastabos. Užduotis atlikite naudodami:
## a) formules, panaudokite alternatyvius hipotezių tikrinimo metodus
##    (statistikos realizacija; p reikšmė; pasikliovimo intervalai);
## b) standartines R funkcijas.
##
## 1. Užduotis
##
## Lentelėje pateikti duomenys apie tam tikros medžiagos kiekį 30
## mėginių:

observ <- c(
    49, 51, 46, 52, 60, 62, 68, 50,
    34, 36, 40, 50, 32, 59, 69, 34,
    48, 44, 40, 42, 42, 45, 53, 40,
    48, 30, 54, 58, 62, 37
)

sample_sd <- sd(observ)
sample_variance <- var(observ)
sample_mean <- mean(observ)

hist(observ, freq = FALSE, col = "gray", main = "Histogram of observed values")
curve(
    dnorm(x, mean = sample_mean, sd = sample_sd),
    col = "tomato", lwd = 3,
    add = TRUE
)
legend(
    "topright",
    legend = c("Normal distribution density"),
    col = c("tomato"),
    lwd = c(3)
)

## Tikrinsime hipotezę
## H_0: populiacijos skirstinys normalusis.
##
shapiro.test(observ)
## 	Shapiro-Wilk normality test
##
## data:  observ
## W = 0.97401, p-value = 0.6536
##
## => Nėra pagrindo atmesti nulinės hipotezės.


## Atlikite užduotis:
## a) Esant 94% patikimumui, įvertinkite medžiagos kiekio standartinį
##    nuokrypį.
##
confidence <- 0.94
alpha <- 1 - confidence

n <- length(observ)

tmp0 <- (n - 1) * sample_variance
sigma_sqr_lo <-  tmp0 / qchisq(alpha / 2, df = n - 1, lower.tail = FALSE)
sigma_sqr_hi <- tmp0 / qchisq(1 - alpha / 2, df = n - 1, lower.tail = FALSE)

data.frame(
    name = c("sigma", "sigma^2"),
    lo = c(sqrt(sigma_sqr_lo), sigma_sqr_lo),
    mid = c(sample_sd, sample_variance),
    hi = c(sqrt(sigma_sqr_hi), sigma_sqr_hi)
)
##      name        lo       mid        hi
## 1   sigma  8.470066  10.54083  13.99399
## 2 sigma^2 71.742013 111.10920 195.83164


## b) Raskite vidurkio pasikliovimo intervalą.
shift <- qt(alpha / 2, df = n - 1, lower.tail = FALSE) * sqrt(sample_variance) / sqrt(n)
mu_lo <- sample_mean - shift
mu_hi <- sample_mean + shift

data.frame(
    name = "mu",
    lo = mu_lo,
    mid = sample_mean,
    hi = mu_hi
)
##   name       lo      mid       hi
## 1   mu 44.06655 47.83333 51.60011

## Galime naudoti t.test() norint rasti pasikliovimo intervalus:
t.test(observ, alternative = 'two.sided', mu = 0, conf.level = confidence)
## 	One Sample t-test

## data:  observ
## t = 24.855, df = 29, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 0
## 94 percent confidence interval:
##  44.06655 51.60011
## sample estimates:
## mean of x
##  47.83333

## c) Patikrinkite prielaidą, kad vidutinis medžiagos kiekis didesnis
##    už 50.
##
## H_0: mu <= mu_0,
## H_1: mu > mu_0.
##
mu_0 <- 50
(t_statistic <- sqrt(n) * (sample_mean - mu_0) / sample_sd)
## > [1] -1.125843

## H_0 atmesime, jei statistika t daugiau už kritinę reikšmę t_lo:
(t_lo <- qt(alpha, df = n - 1, lower.tail = FALSE))
## > [1] 1.601972

(t_statistic > t_lo)
## > [1] FALSE

## => Nėra pagrindo atmesti H_0. Vadinasi, statistiškai, medžiagos
## kiekis mažesnis už 50.

## Tikrinsime naudojantis t.test():
t.test(observ, alternative = "greater", mu = mu_0, conf.level = confidence)
##
## 	One Sample t-test
##
## data:  observ
## t = -1.1258, df = 29, p-value = 0.8653
## alternative hypothesis: true mean is greater than 50
## 94 percent confidence interval:
##  44.75036      Inf
## sample estimates:
## mean of x
##  47.83333

## Remiantis p reikšme, nėra pagrindo atmesti H_0.
