## Pastabos. Užduotis atlikite naudodami:
## a) formules, panaudokite alternatyvius hipotezių tikrinimo metodus
##    (statistikos realizacija; p reikšmė; pasikliovimo intervalai);
## b) standartines R funkcijas.
##
## 6. Užduotis
##
## Pakartotinai matuojant žinomą detalės ilgį tuo pačiu prietaisu 10
## kartų, gautos tokios paklaidų reikšmės:

observ <- c(0.86, 0.06, 1.49, 1.02, 1.39, 0.91, 1.18, -1.50, -0.69, 1.37)

## Tariant, kad matavimo paklaidų skirstinys normalusis reikia
## nustatyti ar matavimo prietaisas neturi sisteminės paklaidos
## (alpha = 0.05).

alpha <- 0.05

## Analizuokime duomenų normalumą:
sample_sd <- sd(observ)
sample_variance <- var(observ)
sample_mean <- mean(observ)
n <- length(observ)

hist(observ, freq = FALSE, col = "gray", main = "Histogram of observed values")
curve(
    dnorm(x, mean = sample_mean, sd = sample_sd),
    col = "tomato", lwd = 3,
    add = TRUE
)
legend(
    "topleft",
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
## W = 0.81996, p-value = 0.02532
##
## => p reikšmė mažesnė už alpha, H_0 atmetame.
##
## Kita vertus, toliau laikysime, kad uždavinio sąlygoje padaryta
## prielaida teisinga.

mu_0 <- 0

## Tikrinsime hipotezę apie vidurkio lygybę mu_0:
##
## H_0: mu = mu_0,
## H_1: mu =/= mu_0.
##
## 1 būdas: Skaičiuosime statistiką ir lyginsime su kritine reikšme:
(test_statistic <- sqrt(n) * abs((sample_mean - mu_0)) / sample_sd)
## > [1] 1.920448

(critical_value <- qt(alpha / 2, df = n - 1))
## > [1] -2.262157

(test_statistic > critical_value)
## > [1] TRUE

## => Kadangi kriterijaus statistika didesnė už kritinę reikšmę, H_0
## atmetame. Vadinasi, statistiškai, vidurkis nelygus nuliui ir
## sisteminė paklaida egzistuoja.


## 2 būdas: Skaičiuosime p-reikšmę:
(p_value <- 2 * (1 - pt(test_statistic, df = n - 1)))
## > [1] 0.08699407

## => p reikšmė didesnė už reikšmingumo lygmenį, todėl nėra pagrindo
## atmesti H_0.


## 3 būdas: Skaičiuosime pasikliovimo intervalą:
shift <- qt(alpha / 2, df = n - 1, lower.tail = FALSE) * sample_sd / sqrt(n)
(mu_lo <- sample_mean - shift)
## > [1] -0.1083607
(mu_hi <- sample_mean + shift)
## > [1] 1.326361

## => mu_0 patenką į pasikliovimo intervalą, todėl nėra pagrindo
## atmesti H_0.


## 4 būdas: R metodas
t.test(
    observ,
    alternative = "two.sided",
    mu = mu_0,
    conf.level = 1 - alpha
)
## 	One Sample t-test
##
## data:  observ
## t = 1.9204, df = 9, p-value = 0.08699
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -0.1083607  1.3263607
## sample estimates:
## mean of x
##     0.609
##
## => p reikšmė didesnė už pasikliovimo lygmenį alpha, todėl nėra
## pagrindo atmesti H_0.
