## 4. Užduotis

set.seed(314)
libary(maxLik)

## Duomenys:

data <- data.frame(
    i = 1:11,
    a_i_1 = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
    a_i = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, Inf),
    U_i = c(8, 12, 19, 23, 29, 30, 25, 18, 15, 14, 18)
)

## Tarę, kad buvo stebėtas a. d., turinti Veibulo skirstinį, įvertinkite
## nežinomus parametrus.
##
## $$
## L(\theta) = \frac{n!}{Z_{n_1}! Z_{n_2}! \ldots Z_{n_N}!}
## {p_1}^{Z_{n_1}}(\theta) {p_2}^{Z_{n_2}}(\theta) \ldots
## {p_N}^{Z_{n_N}}(\theta)
## $$
##
## Čia $Z_{n_i}$ atitinka $U_i$. Naudodami uždavinio sąlygos
## pažymėjimus ir atmettę dėmenis, į kuriuos neįeina parametrai,
## gauname funkciją, kurią reikia maksimizuoti:
##
## $$
## l^* = \sum_{i=1}^k U_i \ln{(p_i (\eta, \nu))}
##     = \sum_{i=1}^k U_i \ln{(F(a_i; \eta, \nu) - F(a_{i-1}; \eta, \nu))},
## $$
##
## čia $F(t; \eta, \nu) = 1 - \exp{\{-{(t / \eta)}^\nu\}}$ yra Veibulo
## pasiskirstymo funkcija, $k$ - intervalų skaičius (11).

k <- 11L

target_f <- function(theta) {
    eta <- theta[1]
    nu <- theta[2]

    N <- k - 1

    s_1 <- data$U_i[1] * log(
        pweibull(data$a_i[1], shape = nu, scale = eta, lower.tail = TRUE)
    )

    s_middle <- sum(
        data$U_i[2:N] * log(
            pweibull(data$a_i[2:N], shape = nu, scale = eta, lower.tail = TRUE)
            - pweibull(data$a_i_1[2:N], shape = nu, scale = eta, lower.tail = TRUE)
        )
    )

    s_k <- data$U_i[k] * log(
        1 - pweibull(data$a_i_1[k], shape = nu, scale = eta, lower.tail = TRUE)
    )

    return(s_1 + s_middle + s_k)
}

## WTF kaip parinkti startai?
mle <- maxLik::maxLik(logLik = target_f, start = c(650, 2))

coef(mle)
## > [1] 649.586088   2.003785
