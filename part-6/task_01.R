## 1 Užduotis.
##
## Lentelėje pateikti duomenys apie tam tikros medžiagos kiekį 30 mėginių:

observ <- c(
    49, 51, 46, 52, 60, 62, 68, 50, 34, 36, 40, 50, 32, 59, 69,
    28, 44, 40, 42, 42, 45, 53, 40, 48, 30, 54, 58, 62, 37
)

## Taikysime ženklų kriterijų (tikrinsime hipotezę apie medianos
## lygybę konkrečiai reikšmei):
##
## H_0: x_0.5 == m = 50,
## H_1: x_0.5 =/= m.
##
##

(median(observ))
## > [1] 48
m <- 50
library(DescTools)
DescTools::SignTest(observ, mu = m)
##
## 	One-sample Sign-Test
##
## data:  observ
## S = 11, number of differences = 27, p-value = 0.4421
## alternative hypothesis: true median is not equal to 50
## 97.6 percent confidence interval:
##  40 53
## sample estimates:
## median of the differences
##                        48
##
## => Esant reikšmingumo lygmeniui alpha = 0.05, nėra pagrindo atmesti
## H_0 ir galime teigti, kad tikroji mediana yra lygi 50.
