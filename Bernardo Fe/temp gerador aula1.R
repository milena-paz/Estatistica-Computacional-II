gerador <- function(n,A=321,B=656,m=727) (A*n+B)%%m

geradorunif <- function(n,inic,mod,plot=F,...)
{
  x <- numeric(n)
  x[1] <- inic
  for(i in 2:n)
    x[i] <- gerador(n=x[i-1],m=mod)
  if(plot)
    plot(x/mod,type="l") 
  return(x/mod)
}
muitonumero <- geradorunif(n=1e4,inic=2,mod=2^31,plot=F)

histograma1 <- hist(muitonumero,breaks=100,freq = F)

boxplot(histograma$counts)

muitonumero2 <- geradorunif(n=1e4,inic=2,mod=2^32,A=1664525,B=1013904223)

histograma2 <- hist(muitonumero2,breaks=100,freq=F)

boxplot(histograma1$counts,histograma2$counts)



prop3 <- runif(1e4)

boxplot(hist(prop3,breaks=100)$counts)

histograma3 <- hist(prop3,breaks=100)


boxplot(histograma1$counts,histograma2$counts,histograma3$counts)


set.seed(666)



valores <- geradorunif(20000,inic=2,mod=2^31,A=65539,B=0)

y <- vetor[(1:10000)*2]

x <- vetor[(1:10000)*2 - 1]

plot(x,y,pch=".")

abline(h=c(0:10)/10,v=c(0:10)/10,col="#555555")

cuty <- cut(y,breaks=10,include.lowest = T)

cutx <- cut(x,breaks=10, include.lowest = T)

tabea <- table(cutx,cuty)

sum(tabea)

tabea[10,10]

set.seed(666)

vetor <- runif(2e4)

plot(x,y,pch=".")

abline(h=c(0:10)/10,v=c(0:10)/10,col="#555555")

for(lin in 1:10)
{
  for(col in 1:10)
  {
    text(x=(lin-0.5)/10,y=(col-0.5)/10,labels=tabea[lin,col],font=2)
  }
}

boxplot(tabea,col="lightblue",cex=2)

soma <- 0

for(i in 1:10)
{
  for(j in 1:10)
  {
    soma <- soma + (tabea[i,j]-100)^2/100
  }
}

soma <- sum((tabea-100)^2/100)

soma
pchisq(q=107.22,100-1)

plot(ecdf(valores))


vetor2 <- geradorunif(1000,inic=2,mod=2^31,A=65539,B=0)

empirica <- ecdf(vetor2)

plot(empirica,verticals=T,lwd=2)

curve(punif,col="red",add=T,lwd=2)

#Kolmogorov Smirnof

ks.test(x=vetor2,y=punif)

set.seed(666)
vetor3 <- runif(50)

empirica3 <- ecdf(vetor3)

plot(empirica3,verticals=T,lwd=2)

curve(punif,col="red",add=T,lwd=2)

ks.test(vetor3,punif)

#testando por conta propria

vetor4 <- geradorunif(1000,inic=92737174,mod=2^31,A=88238370,B=202365602,plot=F)

empirica4 <- ecdf(vetor4)

plot(empirica4,verticals=T,lwd=2)

curve(punif,col="red",add=T,lwd=2)

ks.test(vetor4,punif)

#Gerador 1

#mod=2^31
#A=65539
#B=0
#x_0=2

#Gerador 2

#A=1664525
#B=1013904223
#mod=2^32
#x_0=2

#Gerador 3 é o gerador basico do R
#runif
