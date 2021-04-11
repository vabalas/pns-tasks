## 2 Užduotis:

set.seed(12)
alpha <- 0.05

##
## a) Modeliuokite a.d. X ~ N(mu, sigma_1^2) dydžio n_1 = 100 imtį;
##    a.d. Y ~ N(mu, sigma_2^2) dydžio n_2 = 150 imtį.

mu <- 10
sigma_1 <- 1
sigma_2 <- 5
n_1 <- 100L
n_2 <- 150L
sample_x_a <- rnorm(n_1, mean = mu, sd = sigma_1)
sample_y_a <- rnorm(n_2, mean = mu, sd = sigma_2)

##    Patikrinkite homogeniškumo hipotezę su
##
##    i.  Wilcoxon kriterijumi
##
## Tikriname hipotezę:
##
## H_0: abiejų skirstinių poslinkiai sutampa
## H_1: abiejų skirstinių poslinkiai skiriasi
##
wilcox.test(
    x = sample_x_a,
    y = sample_y_a,
    alternative = "two.sided",
    conf.level = 1 - alpha
)
## 	Wilcoxon rank sum test with continuity correction
##
## data:  sample_x_a and sample_y_a
## W = 7527, p-value = 0.9623
## alternative hypothesis: true location shift is not equal to 0
##
## => Nėra pagrindo atmesti H_0.

##    ii. Ansari - Bradley kriterijumi.
##
##
## Tikriname hipotezę:
##
## H_0: abiejų skirstinių skalės parametras vienodas
## H_1: abiejų skirstinių skalės parametras skiriasi
##
ansari.test(
    x = sample_x_a,
    y = sample_y_a,
    alternative = "two.sided",
    conf.level = 1 - alpha
)
## 	Ansari-Bradley test
##
## data:  sample_x_a and sample_y_a
## AB = 9429, p-value < 2.2e-16
## alternative hypothesis: true ratio of scales is not equal to 1
##
## => H_0 atmetame.

##    Paaiškinkite gautus rezultatus.
## Šiuo atveju tinkamesnis kriterijus turintis skalės alternatyvą.
## Ansari - Bradley kriterijus teisingai parodo kad skirstiniai nėra homogeniški.


## b) Modeliuokit a.d. X ~ N(mu_1, sigma^2) dydžio n_1 = 100 imtį;
##    a.d. Y ~ N(mu_2, sigma^2) dydžio n_2 = 150 imtį.

mu_1 <- 20
mu_2 <- 10
sigma <- 7
sample_x_b <- rnorm(n_1, mean = mu_1, sd = sigma)
sample_y_b <- rnorm(n_2, mean = mu_2, sd = sigma)

##    Patikrinkite homogeniškumo hipotezę su
##
##    i.  Wilcoxon kriterijumi
##
## Tikriname hipotezę:
##
## H_0: abiejų skirstinių poslinkiai sutampa
## H_1: abiejų skirstinių poslinkiai skiriasi
##
wilcox.test(
    x = sample_x_b,
    y = sample_y_b,
    alternative = "two.sided",
    conf.level = 1 - alpha
)
## 	Wilcoxon rank sum test with continuity correction
##
## data:  sample_x_b and sample_y_b
## W = 12806, p-value < 2.2e-16
## alternative hypothesis: true location shift is not equal to 0
##
## => H_0 atmetame.


##    ii. Ansari - Bradley kriterijumi.
##
## Tikriname hipotezę:
##
## H_0: abiejų skirstinių skalės parametras vienodas
## H_1: abiejų skirstinių skalės parametras skiriasi
##
ansari.test(
    x = sample_x_b,
    y = sample_y_b,
    alternative = "two.sided",
    conf.level = 1 - alpha
)
## 	Ansari-Bradley test
##
## data:  sample_x_b and sample_y_b
## AB = 5627, p-value = 0.01626
## alternative hypothesis: true ratio of scales is not equal to 1
##
## => H_0 atmetame.

##    Paaiškinkite gautus rezultatus.
## Šiuo atveju labiau tinkamas kriterijus su poslinkio alternatyva.
## Taigi Vilkoksono kriterijus yra tinkamesnis, ir, iš tiesų, jis
## teisingai atskiria skirstinius, homogeniškumo hipotezę atmesdamas.
