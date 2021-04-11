## 10 Užduotis.
##
## Viename sraute iš 300 stojančiųjų pažymį "1-3" gavo 33, pažymį
## "4-6" gavo 43, pažymį "7-8" gavo 80 ir pažymį "9-10" gavo 144
## stojantieji; kito srauto stojantieji gavo tokius pažymius: pažymį
## "1-3" gavo 39, pažymį "4-6" gavo 35, pažymį "7-8" gavo 72 ir pažymį
## "9-10" gavo 154 stojantieji. Ar galime sakyti, kad abiejų srautų
## stojantieji pasiruošę vienodai?

library(tidyverse)

df <- data.frame(
    Class = c(rep("A", 4), rep("B", 4)),
    Grade = as.factor(rep(c("1-3", "4-6", "7-8", "9-10"), 2)),
    Count = c(33, 43, 80, 144, 39, 35, 72, 154)
)

tbl <- df %>%
    pivot_wider(values_from = Count, names_from = Grade) %>%
    select(-Class)
## # A tibble: 2 x 4
##   `1-3` `4-6` `7-8` `9-10`
##   <dbl> <dbl> <dbl>  <dbl>
## 1    33    43    80    144
## 2    39    35    72    154


## Nagrinėsime dydžių homogeniškumą. Taikysime chi-kvadrato kriterijų
## homogeniškumui tikrinti.
##
## H_0: Dydžiai yra homogeniški
## H_1: Dydžiai nėra homogeniški
##
chisq.test(tbl)
##
## 	Pearson's Chi-squared test
##
## data:  tbl
## X-squared = 2.0771, df = 3, p-value = 0.5566
##
## => H_0 nėra pagrindo atmesti. Iš tiesų,statistiškai, studentai
## pasiruošę vienodai (alpha == 0.05).
