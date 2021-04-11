## 16 Užduotis.
##
## Kompiuterinės sistemos gedimas gali įvykti dėl įvairių priežasčių,
## pvz. programinė įranga, techninė įranga, operatoriaus klaida,
## sistemos perkrova. Manoma, kad 10% gedimų įvyksta dėl programinės
## įrangos, 5% dėl techninės įrangos, 25% d4l operatoriaus klaidos,
## 40% dėl sistemos perkrovų ir 20% dėl kitų priežasčių.
##
## Studijos metu buvo stebėta 150 gedimų ir nustatytos galimos jų
## įvykimo priežastys. Gauta: 13 gedimų dėl programinės įrangos, 10
## dėl techninės įrangos, 42 dėl operatoriaus klaidos, 65 dėl sistemos
## perkrovų ir likę dėl kitų priežasčių.
##
## Ar stebėjimo duomenys neprieštarauja prielaidai?

probs <- c(0.1, 0.05, 0.25, 0.4, 0.2)
observed <- c(13, 10, 42, 65, 20)

## Tikrinsime suderinamumo hipotezę, t.y. tikrinsime ar duomenys
## atitinka konkretų skirstinį.
## Čia tikrinsime paprastąją hipoteze, ta prasme, kad skirstinys yra
## pilnai nusakytas.
##
## Taikysime chi-kvadrato kriterijų suderinamumui tikrinti (turime
## dažnius).
##
## H_0: Dydis suderintas su pateiktu skirstiniu
## H_1: Dydis nėra suderintas
##
chisq.test(
    x = probs,
    y = observed
)
## 	Pearson's Chi-squared test
##
## data:  probs and observed
## X-squared = 20, df = 16, p-value = 0.2202
##
## Warning message:
## In chisq.test(x = probs, y = observed) :
##   Chi-squared approximation may be incorrect
##
## => H_0 nėra pagrindo atmesti. Su 95% patikimumu, galime teigti, kad
## stebėjimai neprieštarauja prielaidai.
