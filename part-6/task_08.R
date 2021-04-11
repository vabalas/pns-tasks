## 8 Užduotis.
##
## Buvo tirta, ar užimamos pareigos ir pasitenkinimas darbu yra
## tarpusavyje susiję dalykai. Atsitiktinai apklausus 800 aukštųjų
## mokyklų dėstytojų buvo gauti rezultatai:


df <- data.frame(
    row.names = c("Happy", "Indifferent", "Unhappy"),
    Assistant = c(40, 78, 57),
    SeniorAssistant = c(60, 87, 63),
    AssociateProfessor = c(52, 82, 66),
    Professor = c(63, 88, 64)
)

## Turime dažnių lentelę. Taikysime chi-kvadrato kriterijų
## nepriklausomumo hipotezei tikrinti.
##
## H_0: Dydžiai nepriklausomi
## H_1: Dydžiai priklausomi
##
chisq.test(df)
##
## 	Pearson's Chi-squared test
##
## data:  df
## X-squared = 2.7506, df = 6, p-value = 0.8394
##
## => H_0 nėra pagrindo atmesti. Statistiškai nėra priklausomybės tarp
## pareigų ir nuomonės (alpha == 0.05).
