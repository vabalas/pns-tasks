## Pastabos. Užduotis atlikite naudodami:
## a) formules, panaudokite alternatyvius hipotezių tikrinimo metodus
##    (statistikos realizacija; p reikšmė; pasikliovimo intervalai);
## b) standartines R funkcijas.
##
## 2. Užduotis
##
## Gamybos procesas suderintas, jeigu parametro A standartinis
## nuokrypis neviršija 10. Atrinkus 100 gaminių buvo pamatuotos jų
## parametro A reikšmės. Ar galite teigti, kad gamybos procesas
## suderintas?

library(EnvStats)

observ <- c(
    24, 36, 29, 36, 40, 25, 19, 29, 58, 50, 41, 30, 37, 25,
    32, 28, 35, 28, 51, 26, 43, 25, 27, 39, 21, 45, 39, 25,
    43, 66, 25, 24, 56, 29, 31, 41, 41, 57, 36, 48, 25, 36,
    48, 24, 48, 22, 7,  31, 24, 32, 53, 33, 46, 22, 33, 37,
    34, 32, 41, 36, 19, 32, 25, 19, 37, 20, 21, 48, 44, 35,
    19, 44, 34, 48, 38, 43, 48, 35, 42, 37, 35, 36, 45, 34,
    40, 37, 21, 41, 11, 41, 27, 24, 37, 39, 33, 45, 39, 43,
    21, 34
)

## Nagrinėsime ar duomenys normalūs.
n <- length(observ)
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
## W = 0.99, p-value = 0.6646
##
## => Nėra pagrindo atmesti nulinės hipotezės.

alpha <- 0.05
sigma_0 <- 10

## Tikrinsime hipotezę
## H_0: sigma^2 <= sigma_0^2;
## H_1: sigma^2 > sigma_0^2.
##

## 1 būdas: lyginsime kritinę reikšmę su kriterijaus statistika, kuri
## turi chi^2(n-1) skirstinį:
(criteria_statistic <- sample_variance * (n - 1) / sigma_0^2)
## > [1] 110.4475
(critical_value <- qchisq(alpha, df = n - 1, lower.tail = FALSE))
## > [1] 123.2252

## Hipotezę H_0 atmesime jei kriterijaus statistika didesnė už kritinę
## reikšmę:
(criteria_statistic > critical_value)
## > [1] FALSE

## => Nėra pagrindo atmesti H_0. Statistiškai, esant reikšmingumo
## lygmeniui alpha, procesas suderintas.


## 2 būdas: Naudojantis pasikliovimo intervalais
(sigma_sqr_lo <- (n - 1) * sample_variance / qchisq(alpha, df = n - 1, lower.tail = FALSE))
## > [1] 89.6306

## H_0 atmesime, jeigu sigma_0 daugiau už apatinį pasikliovimo
## intervalo rėžį:
(sigma_0 > sqrt(sigma_sqr_lo))
## > [1] TRUE

## Turime, kad sigma_0 patenka į intervalą esant pasikliovimo lygmeniui 1 - alpha.
## => Nėra pagrindo atmesti H_0.


## 3 būdas: Suskaičiuosime p reikšmę
(p_value <- 1 - pchisq(criteria_statistic, df = n - 1))
## > [1] 0.2029583

(alpha > p_value)
## > [1] FALSE

## => Nėra pagrindo atmesti H_0, kadangi p reikšmė didesnė už
## reikšmingumo lygmenį alpha.


## 4 būdas:

varTest(
    observ,
    alternative = "greater",
    conf.level = 1 - alpha,
    sigma.squared = sigma_0^2,
    data.name = NULL
)
## 	Chi-Squared Test on Variance
##
## data:  observ
## Chi-Squared = 110.45, df = 99, p-value = 0.203
## alternative hypothesis: true variance is greater than 100
## 95 percent confidence interval:
##  89.6306     Inf
## sample estimates:
## variance
## 111.5631

## => Nėra pagrindo atmesti H_0, kadangi p reikšmė didesnė už
## reikšmingumo lygmenį alpha.
