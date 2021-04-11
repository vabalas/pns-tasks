## 12 Užduotis.
##
## A - išpūstas oro kiekis iš plaučių;
## B - rūkymas. Fišerio tiksliu kriterijumi patikrinkite hipotezę:
## A ir B nepriklausomi.

## Chi kvadrato kriterijus asimptotinis, tinkamas, kai duomenų yra "daug".

tbl <- matrix(
    data = c(2, 16, 4, 64, 83, 46),
    byrow = TRUE,
    ncol = 3,
    dimnames = list(
        c("Abnormal", "Normal"),
        c("Non smokers", "Smokers", "Quit smoking")
    )
)

## Tikrinsime hipotezę:
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
## p-value = 0.019
## alternative hypothesis: two.sided
##
## => H_0 atmetame. Galime sakyti, kad statistiškai A ir B yra
## priklausomi.
