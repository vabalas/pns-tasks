## 4 Užduotis.
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

alpha <- 0.05

## Hipotezė:
##
## H_0: sąryšis nėra (koreliacija == 0),
## H_1: sąryšis yra (koreliacija =/= 0).
##
## Apie koreliacijos kriterijus:
##
## - Pearsono koreliacijos kriterijus tinkamas normaliems duomenims (parametrinis)
## - Spearmano ranginis koreliacijos kriterijus (neparametrinis)
## - Kendallo ranginis koreliacijos kriterijus (neparametrinis)

## Nagrinėkime hipotezę apie populiacijos normalumą:
##
## H_0: dydis yra normalusis,
## H_1: dydis nėra normalusis
##
shapiro.test(df$Mileage)
##
## 	Shapiro-Wilk normality test
##
## data:  df$Mileage
## W = 0.9652, p-value = 0.652
##
## => H_0 atmetame. Vadinasi prielaida apie stebėtų dydžių normalumą
## yra nepagrįsta.

shapiro.test(df$Displacement)
##
## 	Shapiro-Wilk normality test
##
## data:  df$Displacement
## W = 0.89616, p-value = 0.03496
##
## => H_0 nėra pagrindo atmesti.

cor.test(
    x = df$Mileage,
    y = df$Displacement,
    conf.level = 1 - alpha,
    method = "spearman"
)
## 	Spearman's rank correlation rho
##
## data:  df$Mileage and df$Displacement
## S = 2023, p-value = 0.01847
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho
## -0.5210871
##
## Warning message:
## In cor.test.default(x = df$Mileage, y = df$Displacement, conf.level = 1 -  :
##   Cannot compute exact p-value with ties
##
## => H_0 atmetame. Vadinasi, galime teigti, kad sąryšis egzistuoja.


cor.test(
    x = df$Mileage,
    y = df$Displacement,
    conf.level = 1 - alpha,
    method = "kendall"
)
## 	Kendall's rank correlation tau
##
## data:  df$Mileage and df$Displacement
## z = -2.4642, p-value = 0.01373
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##        tau
## -0.4144659
##
## Warning message:
## In cor.test.default(x = df$Mileage, y = df$Displacement, conf.level = 1 -  :
##   Cannot compute exact p-value with ties
##
## => H_0 atmetame. Su 1 - alpha patikimumu, galime teigti, kad
## sąryšis tarp dydžių egzistuoja.
