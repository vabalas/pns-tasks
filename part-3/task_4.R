## Pastabos. Užduotis atlikite naudodami:
## a) formules, panaudokite alternatyvius hipotezių tikrinimo metodus
##    (statistikos realizacija; p reikšmė; pasikliovimo intervalai);
## b) standartines R funkcijas.
##
## 4. Užduotis
##
## Ekonomistas nori patikrinti ar padaugėjo smulkių įmonių
## (procentais). Prieš 10 metų jos sudarė 20% visų įmonių. Šiuo metu
## iš 100 atsitiktinai parinktų įmonių 27 buvo smulkios (alpha =
## 0.05).

alpha <- 0.05
n <- 100
m <- 27
p_before <- 0.2
p_after <- m / n

## Turime binominį skirstinį (dvi galimos įmonių kategorijos).
## Tikrinsime hipotezę:
##
## H_0: p_before <= p_after
## H_1: p_before > p_after
##
## 1 būdas: Konstruosime pasikliovimo intervalą:
## apatinis rėžis yra
(p_lo <- qbeta(1 - alpha, shape1 = m, shape2 =  n - m + 1, lower.tail = FALSE))
## > [1] 0.1979249

## Hipotezę H_0 atmesime, jeigu p_after bus mažesnis už apatinį rėžį
## p_lo:
(p_after > p_lo)
## > [1] TRUE

## => Atmesti H_0 nėra pagrindo. Vadinasi, statistiškai, negalime
## teigti kad smulkių įmonių padaugėjo.

## 2 būdas. R funkcija
binom.test(
    m, n,
    p = p_after,
    alternative = "greater",
    conf.level = 1 - alpha
)
## 	Exact binomial test
##
## data:  m and n
## number of successes = 27, number of trials = 100, p-value = 0.538
## alternative hypothesis: true probability of success is greater than 0.27
## 95 percent confidence interval:
##  0.1979249 1.0000000
## sample estimates:
## probability of success
##                   0.27
##
## => Kadangi p reikšmė yra didesnė už reikšmingumo lygmenį alpha,
## galime teigti, kad smulkių įmonių reikšmingai nepadaugėjo.
