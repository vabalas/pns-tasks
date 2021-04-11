## Pastabos. Užduotis atlikite naudodami:
## a) formules, panaudokite alternatyvius hipotezių tikrinimo metodus
##    (statistikos realizacija; p reikšmė; pasikliovimo intervalai);
## b) standartines R funkcijas.
##
## 9. Užduotis
##
## Vėjo greitis matuotas dviem būdais: nuo žemės, iš
## palydovo. Matavimai atlikti tam tikrais laiko momentais.

df <- data.frame(
    Time = 1:12,
    EarthMeasure = c(
        4.46, 3.99, 3.73, 3.29, 4.82, 6.71,
        4.61, 3.87, 3.17, 4.42, 3.76, 3.30
    ),
    SatMeasure = c(
        4.08, 3.94, 5.00, 5.20, 3.92, 6.21,
        5.95, 3.07, 4.76, 3.25, 4.89, 4.80
    )
)

## Ištirkite sąryšį.


## Pirma patikrinsime ar galime laikyti šiuos stebėjimus
## normaliaisiais.

alpha <- 0.05
n <- nrow(df)

(mean_earth <- mean(df$EarthMeasure))
## > [1] 4.1775
(var_earth <- var(df$EarthMeasure))
## > [1] 0.9299114
(sd_earth <- sd(df$EarthMeasure))
## > [1] 0.9643191

hist(
    df$EarthMeasure,
    freq = FALSE,
    col = "gray",
    main = "Histogram of Earth measured wind speed values"
)
curve(
    dnorm(x, mean = mean_earth, sd = sd_earth),
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
shapiro.test(df$EarthMeasure)
## 	Shapiro-Wilk normality test
##
## data:  df$Displacement
## W = 0.84191, p-value = 0.02921
##
## => H_0 atmetame.


(mean_sat <- mean(df$SatMeasure))
## > [1] 4.589167
(var_sat <- var(df$SatMeasure))
## > [1] 0.9467902
(sd_sat <- sd(df$SatMeasure))
## > [1] 0.9730314

hist(
    df$SatMeasure,
    freq = FALSE,
    col = "gray",
    main = "Histogram of Sat measured wind speed values"
)
curve(
    dnorm(x, mean = mean_sat, sd = sd_sat),
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
shapiro.test(df$SatMeasure)
## 	Shapiro-Wilk normality test
##
## data:  df$Displacement
## W = 0.95976, p-value = 0.7803
##
## => H_0 nėra pagrindo atmesti.

## Matavimus iš palydovo galime laikyti turinčius normalųjį skirstinį.
## Kita vertus, matavai nuo žemės paviršiaus remiantis Shapiro-Wilk
## normalumo kriterijumi nėra normalus.
##
## Nors prielaida nėra pagrįsta, paprastumo dėlei laikysime, kad abu
## dydžiai yra normalūs.

## Toliau nagrinėsime ar abu matavimai būdai vidutiniškai rodo tą patį
## rezultatą, t.y. ar jų vidurkiai yra vienodi.
##
## H_0: beta := mu_1 - mu_2 == 0,
## H_1: beta =/= 0.
##
## Imsime matavimų skirtumus ir tikrinsime hipotezę apie naujo dydžio
## vidurkio lygybę nuliui.
mu_0 <- 0

diff <- df$SatMeasure - df$EarthMeasure
(mean_diff <- mean(diff))
## > [1] 0.4116667
(var_diff <- var(diff))
## > [1] 1.299433
(sd_diff <- sd(diff))
## > [1] 1.139927


## 1 būdas: Skaičiuosime kriterijaus statistiką:
(t_test_statistic <- abs(sqrt(n) * (mean_diff - mu_0) / sd_diff))
## > [1] 1.615042

(t_critical_value <- qt(alpha / 2, df = n - 1, lower.tail = FALSE))
## > [1] 2.093024

## Hipotezę H_0 atmesime jeigu kriterijaus statistika bus didesnė už
## kritinę reikšmę:
(t_test_statistic > t_critical_value)
## > [1] FALSE

## => H_0 atmesti nėra pagrindo, tad remiantis turimais duomenimis, su
## 1 - alpha patikimumu, galime teigti kad vidurkiai reikšmingai
## nesiskiria.


## 2 būdas: Skaičiuosime p reikšmę
(p_value <- 2 * (1 - pt(t_test_statistic, df = n -1)))
## > [1] 0.1227859

## => H_0 atmesti nėra pagrindo.


## 3 būdas: Rasime pasikliovimo intervalą
shift <- t_critical_value * sd_diff / sqrt(n)
(mu_lo <- mean_diff - shift)
## > [1] -0.1218355
(mu_hi <- mean_diff + shift)
## > [1] 0.9451689

## => mu_0 == 0 priklauso šiam intervalui, tad H_0 atmesti neturime
## pagrindo.


## 4 būdas: R metodas
t.test(
    diff,
    alternative = "two.sided",
    mu = mu_0,
    conf.level = 1 - alpha
)
## One Sample t-test
##
## data:  diff
## t = 1.251, df = 11, p-value = 0.2369
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -0.3126083  1.1359417
## sample estimates:
## mean of x
## 0.4116667
##
## => H_0 atmesti neturime pagrindo.
