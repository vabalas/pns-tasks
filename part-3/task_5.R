## Pastabos. Užduotis atlikite naudodami:
## a) formules, panaudokite alternatyvius hipotezių tikrinimo metodus
##    (statistikos realizacija; p reikšmė; pasikliovimo intervalai);
## b) standartines R funkcijas.
##
## 5. Užduotis
##
## Kuriamas naujas kompiuterių tinklas. Reikalaujama, kad jis būtų
## daugiau negu 99% suderintas su jau naudojama įranga. Atlikite
## užduotis:
##
## a) Suformuluokite hipotezę ir alternatyvą.

## Programinė įranga gali būti suderinama su tikimybe p ir
## nesuderinama su tikimybe 1 - p. Turime binominį skirstinį.
## Tikrinsime hipotezę:
##
## H_0: p <= p_0,
## H_1: p > p_0.
##
## Čia p_0 = 0.99

p_0 <- 0.99

## b) Buvo atrinkta 300 programų imtis ir patikrintas jų suderinamumas
##    su kuriamu tinklu. Gauta kad 298 programos suderintos su tinklu. Ar
##    remiantis šiais duomenimis galime atmesti hipotezę?

n <- 300
m <- 298
alpha <- 0.05

## 1 būdas: Konstruosime pasikliovimo intervalą:
## apatinis rėžis yra
(p_lo <- qbeta(1 - alpha, shape1 = m, shape2 =  n - m + 1, lower.tail = FALSE))
## > [1] 0.9791638

## Hipotezę H_0 atmesime, jeigu p_0 bus mažesnis už apatinį rėžį
## p_lo:
(p_0 > p_lo)
## > [1] TRUE

## => Atmesti H_0 nėra pagrindo. Vadinasi, statistiškai, negalime
## teigti tinklas yra suderintas.

## 2 būdas. R funkcija
binom.test(
    m, n,
    p = p_0,
    alternative = "greater",
    conf.level = 1 - alpha
)
## 	Exact binomial test
##
## data:  m and n
## number of successes = 298, number of trials = 300, p-value = 0.4221
## alternative hypothesis: true probability of success is greater than 0.99
## 95 percent confidence interval:
##  0.9791638 1.0000000
## sample estimates:
## probability of success
##              0.9933333
##
## => Kadangi p reikšmė yra didesnė už reikšmingumo lygmenį alpha,
## galime teigti, su patikimumu 1 - alpha, kad kompiuterių tinklas
## nėra suderintas.
