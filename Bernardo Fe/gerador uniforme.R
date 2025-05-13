source("funcoes rng.R")


valores1 <- geradorunif(20000,inic=2,A=65539,B=0,mod = 2^31)

valores2 <- geradorunif(20000,inic=2,A=1664525,B=1013904223,mod = 2^32)

set.seed(666)
valores3 <- runif(2e4)


pares1 <- pareador(valores1)

pares2 <- pareador(valores2)

pares3 <- pareador(valores3)


plotar.tabela(pares1,main="Pontos do primeiro gerador")

plotar.tabela(pares2,main="Pontos do segundo gerador")

plotar.tabela(pares3,main="Pontos do terceiro gerador")


acumuladaempirica(valores1,main="acumulada empirica do primeiro gerador")

acumuladaempirica(valores2,main="acumulada empirica do segundo gerador")

acumuladaempirica(valores3,main="acumulada empirica do terceiro gerador")


ks.test(valores1,punif)

ks.test(valores2,punif)

ks.test(valores3,punif)
