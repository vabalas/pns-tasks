## 15 Užduotis.
##
## Buvo tirtas skirtingų vaistų dozių efektyvumas. Vienoje grupėje
## duodama mažesnė vaistų dozė, antroje didesnė, o trečia grupė gavo
## placebo (kontrolinė grupė). Tyrimas buvo atliekamas mėnesį. Po
## gydymo kurso buvo įvertintas gydymo efektyvumas (užkoduotas
## skaičiais nuo 1 iki 8; 1 - gydymas visai nepadėjo, 8 - visiškai
## pasveiko). Ar visose grupėse išgijimas vienodas?

alpha <- 0.05
group_1 <- c(5, 4, 1, 7, 4, 3, 6, 7, 8, 7)
group_2 <- c(5, 8, 2, 8, 7, 4, 5, 4, 6, 4)
group_3 <- c(5, 3, 7, 1, 2, 4, 2, 1, 4, 5, 4, 5)

## Tikrinsime homogeniškumo hipotezę. Taikysime Kruskalio ir Voliso
## ranginį kriterijų nepriklausomoms imtims.
##
## H_0: Visose grupės skirstiniai vienodi
## H_1: Bent dviejų grupių skirstiniai skirtingi
##
kruskal.test(
    x = list(
        group_1,
        group_2,
        group_3
    )
)
## 	Kruskal-Wallis rank sum test
##
## data:  list(group_1, group_2, group_3)
## Kruskal-Wallis chi-squared = 4.4737, df = 2, p-value = 0.1068
##
## => H_0 nėra pagrindo atmesti. Statistiškai, visose grupėse
## išgyjimas yra vienodas, esant alpha == 0.05 reikšmingumo lygmeniui.
