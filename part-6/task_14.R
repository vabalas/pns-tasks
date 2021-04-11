## 14 Užduotis.
##
## Buvo testuotas dviejose gamyklose pagamintų televizorių darbo
## laikas iki pirmo gedimo. Ar galima tvirtinti, kad abiejose
## gamyklose pagamintų televizorių darbo trukmė skiriasi? Jei skirasi,
## tai kurios gamyklos gaminių vidutinis darbo laikas ilgesnis?

alpha <- 0.05

factory_a <- c(41, 70, 26, 89, 62, 54, 46, 77, 34, 51)
mean(factory_a)
## > [1] 55
factory_b <- c(23, 35, 29, 38, 21, 53, 31, 25, 36, 50, 61)
mean(factory_b)
## > [1] 36.54545

## Tikrinsime homogeniškumo hipotezę, su poslinkio
## alternatyva. Taikysime Vilkoksono kriterijų.
##
## H_0: m_1 <= m_2
## H_1: m_1 > m_2
##
wilcox.test(
    x = factory_a,
    y = factory_b,
    alternative = "greater",
    conf.level = 1 - alpha
)
##
## 	Wilcoxon rank sum exact test
##
## data:  factory_a and factory_b
## W = 87, p-value = 0.01208
## alternative hypothesis: true location shift is greater than 0
##
## => H_0 atmetame. Statistiškai, su 1 - alpha patikimumu, pirmosios
## gamyklos produktų veikimo laikas ilgesnis nei antrosios.
