## Pastabos. Užduotis atlikite naudodami:
## a) formules, panaudokite alternatyvius hipotezių tikrinimo metodus
##    (statistikos realizacija; p reikšmė; pasikliovimo intervalai);
## b) standartines R funkcijas.
##
## 2. Užduotis
##
## Gamybos procesas suderintas, jeigu parametro A standartinis
## nuokrypis neviršija 10. Atrinkus 100 gaminių buvo pamatuotos jų
## parametro A reikšmės. Ar galite teigti, kad gamybos procesas
## suderintas?

observ <- c(
    24, 36, 29, 36, 40, 25, 19, 29, 58, 50, 41, 30, 37, 25,
    32, 28, 35, 28, 51, 26, 43, 25, 27, 39, 21, 45, 39, 25,
    43, 66, 25, 24, 56, 29, 31, 41, 41, 57, 36, 48, 25, 36,
    48, 24, 48, 22, 7,  31, 24, 32, 53, 33, 46, 22, 33, 37,
    34, 32, 41, 36, 19, 32, 25, 19, 37, 20, 21, 48, 44, 35,
    19, 44, 34, 48, 38, 43, 48, 35, 42, 37, 35, 36, 45, 34,
    40, 37, 21, 41, 11, 41, 27, 24, 37, 39, 33, 45, 39, 43,
    21, 34
)
