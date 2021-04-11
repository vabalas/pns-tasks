## 9 Užduotis.
##
## Atliktas tyrimas, ar vitamino C kasdienis vartojimas padeda
## išvengti peršalimo. Atsitiktinai apklausus 100 žmonių, ar jie buvo
## peršalę praėjusiais metais, gauti tokie rezultatai:

df <- data.frame(
    row.names = c("Used vit. c", "Did not use vit. c"),
    SickCount = c(10, 21),
    HealthyCount = c(20, 14)
)

## Ar kasdieninis vitamino C vartojimas apsaugo nuo peršalimo?

## Turime dažnių lentelę. Taikysime chi-kvadrato kriterijų
## nepriklausomumo hipotezei tikrinti.
##
## H_0: Dydžiai nepriklausomi
## H_1: Dydžiai priklausomi
##
chisq.test(df)
##
## 	Pearson's Chi-squared test with Yates' continuity correction
##
## data:  df
## X-squared = 3.5978, df = 1, p-value = 0.05786
##
## => H_0 nėra pagrindo atmesti. Statistiškai nėra priklausomybės tarp
## vitamino C vartojimo ir sirgimo (alpha == 0.05).
