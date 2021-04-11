## 5 Užduotis.
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

alpha <- 0.05

## Tirsime dydžių homogeniškumą.

## Tikrinsime ar stebėti dydžiai yra homogeniški.
##
## Hipotezė apie homogeniškumą su poslinkio alternatyva:
##
## H_0: abiejų skirstinių poslinkiai sutampa
## H_1: abiejų skirstinių poslinkiai skiriasi
##
wilcox.test(
    x = df$EarthMeasure,
    y = df$SatMeasure,
    alternative = "two.sided",
    conf.level = 1 - alpha
)
## 	Wilcoxon rank sum exact test
##
## data:  df$EarthMeasure and df$SatMeasure
## W = 49, p-value = 0.1978
## alternative hypothesis: true location shift is not equal to 0
##
## => H_0 nėra pagrindo atmesti. Vadinasi, hipotezės kad dydžių
## skirstinių poslinkiai sutampa, negalime atmesti.

## Hipotezė apie homogeniškumą su skalės alternatyva:
##
## H_0: abiejų skirstinių skalės parametras vienodas
## H_1: abiejų skirstinių skalės parametras skiriasi
##
ansari.test(
    x = df$SatMeasure,
    y = df$EarthMeasure,
    alternative = "two.sided",
    conf.level = 1 - alpha
)
## 	Ansari-Bradley test
##
## data:  df$SatMeasure and df$EarthMeasure
## AB = 72, p-value = 0.531
## alternative hypothesis: true ratio of scales is not equal to 1
##
## => H_0 nėra pagrindo atmesti. Vadinasi, hipotezės, kad dydžiai yra
## homogeniški skalės parametro prasme, negalime atmesti.
