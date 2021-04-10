## Pastabos. Užduotis atlikite naudodami:
## a) formules, panaudokite alternatyvius hipotezių tikrinimo metodus
##    (statistikos realizacija; p reikšmė; pasikliovimo intervalai);
## b) standartines R funkcijas.
##
## 3. Užduotis
##
## (Milton J. S., Arnold J. C. Probability and Statistics Engineering
## and Computing Sciences. Chapter 7, Exercice 3.)

observ <- c(8, 5, 0, 10, 0, 3, 1, 12, 2, 7, 9, 6)

## Ar galime teigti, kad vidutinis defektų skaičius viename
## kvadratiniame metre didesnis už 6 (reikšmingumo lygmuo 0.1)?

alpha <- 0.1
lambda_0 <- 6

## Analizuokime duomenų pasiskirstymą:
n <- length(observ)
(sample_variance <- var(observ))
## > [1] 16.56818
(sample_mean <- mean(observ))
## > [1] 5.25

hist(observ, freq = FALSE, col = "gray", main = "Histogram of observed values")
points(
    sort(observ),
    dpois(sort(observ), lambda = sample_mean),
    col = "tomato", lwd = 2,
    add = TRUE
)
legend(
    "topright",
    legend = c("Poisson distribution probability"),
    col = c("tomato"),
    lty = 0, pch = 1, lwd = 2
)

## Nėra akivaizdu, kad šie stebėjimai turi Puasono skirstinį, todėl
## turėsime palyginti teorines ir empirines tikimybes.
qqplot(observ, rpois(n, lambda = sample_mean), ylim = c(0, 11))
abline(a = 0, b = 1, col = "blue")

## Galutinai nėra aišku, tačiau esant nedidelei imčiai, suderinamumą
## nustatyti visiškai tiksliai negalime, todėl laikykime imtis yra iš
## Puasono skirstinio.

## Toliau, tikrinsime hipotezę:
##
## H_0: lambda <= lambda_0;
## H_1: lambda > lambda_0.
##
## Konstruosime pasikliovimo intervalą parametrui lambda:
## apatinis rėžis yra
(lambda_lo <- qchisq(1 - alpha, df = 2 * sum(observ), lower.tail = FALSE) / (2 * n))
## [1] 4.422173

## Hipotezę H_0 atmesime, jeigu lambda_0 yra mažesnė už apatinį
## pasikliovimo intervalo rėžį:
(lambda_0 < lambda_lo)
## > [1] FALSE

## => Nėra pagrindo atmesti H_0, todėl, statistiškai, teigti, kad
## defektų skaičius yra didesnis negu 6, negalime.
