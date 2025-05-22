source("funcoes rng.R")

set.seed(666)

normal <- geradorcirculo(1000)$pontos

hist(normal,freq =F)

plot(ecdf(normal))
curve(pnorm(x),add=T,col="red")

ks.test(normal,pnorm)

qqnorm(normal)
qqline(normal,col="red",lwd=3)



amostra <- rexp(100)

qqnorm(amostra)
qqline(amostra,lwd=3,col="red")


amostra2 <- rcauchy(1000)

qqnorm(amostra2)
qqline(amostra2,lwd=3,col="red")


amostra3 <- rt(100,df=3)

qqnorm(amostra3)
qqline(amostra3 ,lwd =3 ,col="red")


set.seed(666)
x <- rnorm(10)
rank(x)
x[rank(x)]

probs <- rank(x)/length(x)

q.amostra <- x
q.teorico <- qnorm(probs,mean(q.amostra),sd(q.amostra))

plot(x=q.teorico,y=q.amostra,main="QQ-plot normal",xlab="Quantis teóricos",ylab="Quantis empíricos")

