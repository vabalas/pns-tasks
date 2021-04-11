## 6 Užduotis.
##
## Buvo tirtas darbo našumo padidėjimas firmose, gaminančiose
## elektroninę skaičiavimo įrangą. Firmos buvo suskirstytos
## atsižvelgiant į vidutines išlaidas (mažos, vidutinės, didelės),
## skiriamas moksliniams tyrimams ir naujoms technologijoms per
## paskutinius tris metus. Rezultatai pateikti lentelėje (darbo našumo
## pagerėjimas balais):

sample_small <- c(7.6, 8.2, 6.8, 5.8, 6.9, 6.6, 6.3, 7.7, 6.0)
sample_average <- c(6.7, 8.1, 9.4, 8.6, 7.8, 7.7, 8.9, 7.9, 8.3, 8.7, 7.1, 8.4)
sample_large <- c(8.5, 9.7, 10.1, 7.8, 9.6, 9.5)

alpha <- 0.05

## Ar galime teigti, kad moksliniams tyrimams ir naujoms
## technologijoms skiriamos lėšos turi įtakos darbo našumui?

## Nagrinėsime hipotezę ar dydžių skirstiniai yra skirtingi:
##
## H_0: Visi skirstiniai sutampa,
## H_1: Bent du skiriasi
##
kruskal.test(
    x = list(
        sample_small,
        sample_average,
        sample_large
    )
)
##
## 	Kruskal-Wallis rank sum test
##
## data:  list(sample_small, sample_average, sample_large)
## Kruskal-Wallis chi-squared = 14.956, df = 2, p-value = 0.0005655
##
## => H_0 atmetame. Esant reiškmigumo lygmeniui alpha == 0.05, galime
## teigti kad skirstiniai skiriasi, ir papildomas investavimas turėjo
## įtakos įmonių veiklai.
