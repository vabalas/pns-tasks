## Pastabos. Užduotis atlikite naudodami:
## a) formules, panaudokite alternatyvius hipotezių tikrinimo metodus
##    (statistikos realizacija; p reikšmė; pasikliovimo intervalai);
## b) standartines R funkcijas.
##
## 8. Užduotis
##
## Ištirkite automobilio variklio tūrio ir nuvažiuoto automagistralėje
## atstumo (su vienodu kiekiu degalų) sąryšį (D. C. Montgomery,
## R. C. Runger. Applied Statistics and Probability for Engineers. 3th
## ed. 2003, p. 381).

df <- data.frame(
    Brand = c(
        "Acura", "BMW", "Buick", "Chevrolet", "Chevrolet", "Chrysler",
        "Dodge", "Dodge", "Ford", "Ford", "Ford", "Ford", "Honda", "Mazda",
        "Mercedes", "Mercury", "Nissan", "Oldsmobile", "Plymouth",
        "Pontiac"
    ),
    Make = c(
        "Legend", "735i", "Regal", "Cavalier", "Celebrity", "Conquest",
        "Aries", "Dynasty", "Escort", "Mustang", "Taurus", "Tempo",
        "Accord", "RX-7", "260E", "Tracer", "Maxima", "Cutlass", "Laser",
        "GrandPrix"
    ),
    Mileage = c(
        30, 19, 29, 32, 30, 24, 30, 28, 31, 25, 27, 33, 30, 23, 24, 29, 26,
        29, 37, 29
    ),
    Displacement = c(
        97, 209, 173, 121, 151, 156, 135, 181, 114, 302, 153, 90, 119, 80,
        159, 97, 181, 173, 122, 173
    )
)

n <- nrow(df)

## Tarsime, kad buvo stebimi normalieji atsitiktinai dydžiai.
(mean_displacement <- mean(df$Displacement))
## > [1] 149.3
(var_displacement <- var(df$Displacement))
## > [1] 2549.274
(sd_displacement <- sd(df$Displacement))
## > [1] 50.49033

hist(
    df$Displacement,
    freq = FALSE,
    col = "gray",
    main = "Histogram of displacement values"
)
curve(
    dnorm(x, mean = mean_displacement, sd = sd_displacement),
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
shapiro.test(df$Displacement)
## 	Shapiro-Wilk normality test
##
## data:  df$Displacement
## W = 0.89616, p-value = 0.03496
##
## => H_0 atmetame.


(mean_mileage <- mean(df$Mileage))
## > [1] 28.25
(var_mileage <- var(df$Mileage))
## > [1] 15.88158
(sd_mileage <- sd(df$Mileage))
## > [1] 3.98517

hist(
    df$Mileage,
    freq = FALSE,
    col = "gray",
    main = "Histogram of mileage values"
)
curve(
    dnorm(x, mean = mean_mileage, sd = sd_mileage),
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
shapiro.test(df$Mileage)
## 	Shapiro-Wilk normality test
##
## data:  df$Displacement
## W = 0.9652, p-value = 0.652
##
## => Nėra pagrindo atmesti H_0.

## Nors prielaida kad Displacement stebėjimai yra normalūs nėra
## pagrįsta, toliau laikysime, kad abu stebėti dydžiai yra normalūs.

plot(df$Displacement, df$Mileage)

## Tikrinsime hipotezę ar koreliacija tarp nuvažiuoto atstumo ir
## variklio tūrio yra neigiama:
##
## H_0: rho >= 0,
## H_1: rho < 0.
##
## 1 būdas: Skaičiuosime kritinę reikšmę:

(r <- cor(df$Mileage, df$Displacement))
## > [1] -0.4484653

(t_test_statistic <- sqrt(n - 2) * r / sqrt(1 - r^2))
## > [1] -2.12875

(t_critical_value <- -qt(alpha, df = n - 2, lower.tail = FALSE))
## > [1] -2.12875

## H_0 atmesime jeigu kriterijaus statistika yra mažesnė už kritinę
## reikšmę:
(t_test_statistic < t_critical_value)
## > [1] TRUE

## => Hipotezę H_0 atmetame. Esant 1 - alpha patikimumui, galime
## teigti, kad egzistuoja neigiama koreliacija tarp nuvažiuoto atstumo
## ir variklio tūrio.


## 2 būdas: Skaičiuosime p reikšmę:
(p_value <- pt(t_test_statistic, df = n - 2))
## > [1] 0.02367019

## => Kadangi p reikšmė mažesnė už reikšmingumo lygmenį alpha, H_0
## atmetame.


## 3 būdas: Naudosimės R metodais:
cor.test(
    df$Displacement, df$Mileage,
    alternative = "less",
    method = "pearson"
)
## 	Pearson's product-moment correlation
##
## data:  df$Displacement and df$Mileage
## t = -2.1287, df = 18, p-value = 0.02367
## alternative hypothesis: true correlation is less than 0
## 95 percent confidence interval:
##  -1.00000000 -0.08364601
## sample estimates:
##        cor
## -0.4484653
##
## => Kadangi p reikšmė mažesnė už reikšmingumo lygmenį alpha, H_0
## atmetame.
