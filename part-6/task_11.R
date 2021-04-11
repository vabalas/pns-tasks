## 11 Užduotis.
##
## Patikrinkite hipotezę, kad tam tikro dalyko pažymiai atestate ir
## per stojamuosius egzaminus yra nepriklausomi. X_i pažymys atestate,
## Y_i - pažymys, gautas per stojamuosius egzaminus.

tbl <- matrix(
    data = c(110, 70, 60, 10, 0, 10, 10, 30),
    dimnames = list(
        c("7-10", "4-6"),
        c("9-10", "7-8", "4-6", "1-3")
    ),
    ncol = 4,
    byrow = TRUE
)

## Galėtume naudoti chi-kvadrato kriterijų nepriklausomumui tikrinti,
## tačiau stebėjome 0. Homogeniškumo hipotezei tikrinti taikysime
## Fišerio kriterijų.
##
## H_0: Dydžiai nepriklausomi,
## H_1: Dydžiai priklausomi.
##
alpha <- 0.05
fisher.test(
    tbl,
    alternative = "two.sided",
    conf.level = 1 - alpha
)
## 	Fisher's Exact Test for Count Data
##
## data:  tbl
## p-value < 2.2e-16
## alternative hypothesis: two.sided
##
## => H_0 galime atmesti. Dydžiai yra statistiškai reikšmingai
## priklausomi.
