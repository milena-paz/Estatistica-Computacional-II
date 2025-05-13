

#N sei oq é o qqplot exatamente, so fiz pq ele mandou
set.seed(666)
amostra <- rnorm(30)
qqnorm(amostra)
qqline(amostra)

#auto correlação em intervalos
set.seed(666)
amostra2 <- runif(1000)
acf(amostra2)

#tentando fazer uma uniforme com 12 gerar uma normal
uniformes <- runif(12)

plot(ecdf(uniformes))

uniformes.centradas <- uniformes - 0.5

plot(ecdf(uniformes.centradas))

abline(0.5,1)

Z <- sum(uniformes.centradas)

set.seed(666)
#Matriz com 1000 uniformes de 12
matunif <- matrix(data=replicate(1e3,runif(12))-0.5,nrow=1000,ncol=12)

#vetor com a soma das linhas do matunif
matNorm <- apply(matunif,1,sum)

F.star <- ecdf(matNorm)

plot(F.star,col="red",lwd=1.5)
curve(pnorm,add=T,lwd=2)

ks.test(matNorm,pnorm)

quantis <- qnorm(c(0:10)/10)

cut.star <- cut(matNorm,breaks=quantis)

table(cut.star)

quiquadrado <- sum( - 100)^2/100

pchisq()

normal.pareada <- pareador(matNorm)

plot(normal.pareada$x,normal.pareada$y)
