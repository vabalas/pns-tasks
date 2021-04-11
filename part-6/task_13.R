## 13 Užduotis.
##
## Reklamos agentūra atliko tyrimą, norėdama nustatyti ar skalbimo miltelių nemokamas dalinimas bandymams namie turi įtakos potencialių pirkėjų pasirinkimui. Atsitiktinai apklausti 100 pirkėjų turėjo pasakyti, kurie skalbimo milteliai A ar B geresni. Pakartotinai savo nuomonę pirkėjai pareiškė abiejų rūšių miltelius išbandę namie. Buvo gauti rezultatai:

tbl <- matrix(
    data = c(41, 3, 9, 47),
    ncol = 2,
    byrow = TRUE
)

## Ar skalbimo miltelių bandymas namuo turėjo įtakos pirkėjų nuomonei?


## Turime dažnių lentelę ir priklausomas imtis.  Taikysime Maknemaro
## kriterijų (Pastebėkime, kad chi-kvadrato kriterijus homogeniškumo
## tikrinimui yra skirtas nepriklausomoms imtims).
##
## H_0: Dydžių skirstiniai vienodi
## H_1: Dydžių skirstiniai skiriasi
##
mcnemar.test(tbl)
##
## 	McNemar's Chi-squared test with continuity correction
##
## data:  tbl
## McNemar's chi-squared = 2.0833, df = 1, p-value = 0.1489
##
## => Nėra pagrindo atmesti H_0. Galime teigti, su 1 - alpha
## patikimumu, kad bandymas turėjo įtakos pirkėjų nuomonei.
