## Pastabos. Užduotis atlikite naudodami:
## a) formules, panaudokite alternatyvius hipotezių tikrinimo metodus
##    (statistikos realizacija; p reikšmė; pasikliovimo intervalai);
## b) standartines R funkcijas.
##
## 7. Užduotis
##
## Tyrimo tikslas palyginti laiką, kuris reikalingas patikrinti laidų
## sujungimus ir izoliaciją dviejų tipų srovės
## pertraukikliuose. Pirmoji populiacija susideda iš visų vakuumo tipo
## srovės pertraukiklių, o antroji populiacija iš visų oro-magnetinių
## srovės pertraukiklių. Buvo atrinktos paprastosios atsitiktinės
## imtys iš kiekvienos populiacijos ir ištirtas kiekvienas į imtį
## patekęs gaminys (matuotas laikas).

## Vakuumo tipo
vacuum <- c(3.0, 5.3, 6.9, 4.1, 8.0, 6.7, 6.3, 7.1, 4.2, 7.2, 5.1, 5.5, 5.8)
(mean_vacuum <- mean(vacuum))
## > [1] 5.784615
(var_vacuum <- var(vacuum))
## > [1] 2.073077
n_vacuum <- length(vacuum)

## Oro-magnetinis
airmag <- c(7.1, 9.3, 8.2, 10.4, 9.1, 8.7, 12.1, 10.7, 10.6, 10.5, 11.3, 11.5)
(mean_airmag <- mean(airmag))
## > [1] 9.958333
(var_airmag <- var(airmag))
## > [1] 2.202652
n_airmag <- length(airmag)

## Tarkime, kad buvo stebėti normalieji atsitiktiniai dydžiai.

## Tikrinsime hipotezę ar populiacijų dispersijos lygios
##
## H_0: k := sigma_1^2 / sigma_2^2 == k_0 (k_0 = 1),
## H_1: k =/= k_0 (k_0 = 1)
##
## 1 būdas: Skaičiuosime kritinę reikšmę (F statistika):
k_0 <- 1
(f_test_statistic <- var_vacuum / (var_airmag * k_0))
## > [1] 0.9411734

(f_critical_value_lo <- qf(alpha / 2, df1 = n_vacuum - 1, df2 = n_airmag - 1, lower.tail = FALSE))
## > [1] 3.429613
(f_critical_value_hi <- qf(1 - alpha / 2, df1 = n_vacuum - 1, df2 = n_airmag - 1, lower.tail = FALSE))
## > [1] 0.3010705

## => Kadangi F statistika didesnė už 1 - alpha/2 kritinę reikšmę ir
## mažesnė už alpha/2 kritinę reikšmę, negalime atmesti H_0.
## Statistikškai, esant patikimumui 1 - alpha, dispersijos yra lygios.


## 2 būdas: Suskaičiuosime p reikšmę:
(p_value <- 2 * min(c(
    1 - pf(f_test_statistic, df1 = n_vacuum - 1, df2 = n_airmag - 1),
    pf(f_test_statistic, df1 = n_vacuum - 1, df2 = n_airmag - 1)
)))
## > [1] 0.9130552

## => Kadangi p reikšmė didesnę už reikšmingumo lygmenį alpha, nėra
## pagrindo atmesti H_0.


## 3 būdas: R funkcija
var.test(vacuum, airmag, alternative = "two.sided")
##
## 	F test to compare two variances
##
## data:  vacuum and airmag
## F = 0.94117, num df = 12, denom df = 11, p-value = 0.9131
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.2744255 3.1260897
## sample estimates:
## ratio of variances
##          0.9411734
##
## => Kadangi p reikšmė didesnę už reikšmingumo lygmenį alpha, nėra
## pagrindo atmesti H_0.


## Tikrinsime hipotezę ar vakuuminių įtaisų laiko vidurkis didesnis už
## oro-magnetinių:
##
## H_0*: beta := mu_1 - mu_2 >= 0,
## H_1*: beta < 0.
##
## 1 būdas: Skaičiuosime kritinę reikšmę
mu_0 <- 0

tmp0 <- (mean_vacuum - mean_airmag - mu_0)
tmp1 <- sqrt(var_vacuum * (n_vacuum - 1) + var_airmag * (n_airmag - 1))
tmp2 <- sqrt((n_vacuum * n_airmag * (n_vacuum + n_airmag - 2)) / (n_vacuum + n_airmag))
(t_test_statistic <- tmp0 / tmp1 * tmp2)
## > [1] -7.13529

tmp0 <- (var_vacuum / n_vacuum + var_airmag / n_airmag)^2
tmp1 <- (var_vacuum / n_vacuum)^2 / (n_vacuum - 1)
tmp2 <- (var_airmag / n_airmag)^2 / (n_airmag - 1)
(nu <- floor(tmp0 / (tmp1 + tmp2)))
## > [1] 22

(t_critical_value <- -qt(alpha, df = nu, lower.tail = TRUE))
## > [1] 1.717144
## => Kadangi kriterijaus statistika mažesnė už kritinę reikšmę, H_0
## atmetame. Vadinasi, statistiškai, oro-magnetinių srovės
## pertraukliklių veikimo laiko vidurkis yra didesnis.


## 2 būdas: R funkcija
t.test(vacuum, airmag, alternative = "less",  var.equal = TRUE)
## 	Two Sample t-test
##
## data:  vacuum and airmag
## t = -7.1353, df = 23, p-value = 1.44e-07
## alternative hypothesis: true difference in means is less than 0
## 95 percent confidence interval:
##       -Inf -3.171206
## sample estimates:
## mean of x mean of y
##  5.784615  9.958333
##
## => p reikšmė mažesnė už pasikliovimo lygmenį alpha todėl H_0
## atmetame.

## 3 būdas: Skaičiuosime p reikšmę:
(p_value <- 2 * (1 - pt(abs(t_test_statistic), df = n_vacuum + n_airmag - 2)))
## > [1] 2.87992e-07

## => p reikšmė mažesnė už pasikliovimo lygmenį alpha todėl H_0
## atmetame.
