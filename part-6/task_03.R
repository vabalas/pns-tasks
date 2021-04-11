## 3 Užduotis.
##
## Tyrimo tikslas palyginti laiką, kuris reikalingas patikrinti laidų
## sujungimus ir izoliaciją dviejų tipų srovės
## pertraukikliuose. Pirmoji populiacija susideda iš visų vakuumo tipo
## srovės pertraukiklių, o antroji populiacija iš visų oro-magnetinių
## srovės pertraukiklių. Buvo atrinktos paprastosios atsitiktinės
## imtys iš kiekvienos populiacijos ir ištirtas kiekvienas į imtį
## patekęs gaminys (matuotas laikas).

## Vakuumo tipo
vacuum <- c(3.0, 5.3, 6.9, 4.1, 8.0, 6.7, 6.3, 7.1, 4.2, 7.2, 5.1, 5.5, 5.8)

## Oro-magnetinis
airmag <- c(7.1, 9.3, 8.2, 10.4, 9.1, 8.7, 12.1, 10.7, 10.6, 10.5, 11.3, 11.5)

alpha <- 0.05

## Tikrinsime ar stebėti dydžiai yra homogeniški.
##
## Hipotezė apie homogeniškumą su poslinkio alternatyva:
##
## H_0: abiejų skirstinių poslinkiai sutampa
## H_1: abiejų skirstinių poslinkiai skiriasi
##
wilcox.test(
    x = vacuum,
    y = airmag,
    alternative = "two.sided",
    conf.level = 1 - alpha
)
## 	Wilcoxon rank sum test with continuity correction
##
## data:  vacuum and airmag
## W = 2.5, p-value = 4.499e-05
## alternative hypothesis: true location shift is not equal to 0
##
## Warning message:
## In wilcox.test.default(x = vacuum, y = airmag, alternative = "two.sided",  :
##   cannot compute exact p-value with ties
##
## => H_0 atmetame, vadinasi, su 1 - alpha patikimumu, galime teigti
## kad dydžių skirstinių poslinkiai skiriasi.


## Hipotezė apie homogeniškumą su skalės alternatyva:
##
## H_0: abiejų skirstinių skalės parametras vienodas
## H_1: abiejų skirstinių skalės parametras skiriasi
##
ansari.test(
    x = vacuum,
    y = airmag,
    alternative = "two.sided",
    conf.level = 1 - alpha
)
## 	Ansari-Bradley test
##
## data:  vacuum and airmag
## AB = 91.5, p-value = 0.6942
## alternative hypothesis: true ratio of scales is not equal to 1
##
## Warning message:
## In ansari.test.default(x = vacuum, y = airmag, alternative = "two.sided",  :
##   cannot compute exact p-value with ties
##
## => H_0 nėra pagrindo atmesti, tad, statistiškai, nagrinėjamų dydžių
## skalės parametras nesiskiria.

## Galime apibendrinti, kad poslinkio prasme šie dydžiai nėra
## homogeniški.
