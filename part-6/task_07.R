## 7 Užduotis.
##
## Tarkime, kad mažmeninės prekybos firma nori pasirinkti spausdintuvų
## tiekėją. Ši firma gavo iš trijų tiekėjų informaciją apie 12
## spausdintuvų kainas, kurios pateiktos lentelėje. Ar tiekėjų
## siūlomos spausdintuvų kainos skiriasi?

alpha <- 0.05
df <- data.frame(
    Printer = rep(1:12, 3),
    Supplier = as.factor(c(
        rep("Supplier A", 12),
        rep("Supplier B", 12),
        rep("Supplier C", 12)
    )),
    Price = c(
        660, 790, 590, 950, 1290, 1550, 1980, 2300, 2500, 2190, 5590, 6000,
        673, 799, 580, 945, 1280, 1500, 1950, 2295, 2480, 2199, 5500, 6100,
        658, 785, 599, 960, 1295, 1499, 1970, 2310, 2490, 2210, 5550, 6090
    )
)

## Tikrinsime homogeniškumo hipotezę. Kadangi turime priklausomas
## imtis, taikysime Friedmano ranginį kriterijų.
##
## H_0: Skirstiniai vienodi,
## H_1: Skirstiniai skiriasi.
##
friedman.test(
    Price ~ Printer | Supplier,
    data = df
)
## 	Friedman rank sum test
##
## data:  Price and Printer and Supplier
## Friedman chi-squared = 33, df = 11, p-value = 0.0005262
##
## => H_0 atmetame. Galime teigti, su 1 - alpha patikimumu, kad
## skirstiniai skyriasi. Vadinasi, kainos pas tiekėjus skirasi
## statistiškai reikšmingai.
