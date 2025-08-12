## Suavização ##
# É usada para descobrir tendências em dados com ruído; não-lineares e com
# mudanças na estrutura da tendência

# Suavizadores mais utilizados:
# - Loess
# - Núcleo estimador (kernel)
# - Splines

# para estabelecer uma função densidade a partir dos dados, uma primeira tentativa
# seria baseada em seu histograma.

hist(faithful$waiting,freq=F)
# nH=12, quantidade escolhida pelo método Sturges
hist(faithful$waiting,freq=F,breaks=32)
# nH=32, vemos mais ruido na visualização
hist(faithful$waiting,freq=F,breaks=4)
# nH=4, informação é perdida por conta da excessiva suavização

par(mfrow=c(3,1))
# metodo Sturges
hist(faithful$waiting,freq=F)
#metodo freedman-diaconis
hist(faithful$waiting, breaks="Freedman-Diaconis")
2*IQR(faithful$waiting)/272^(1/3)
# metodo Scott
hist(faithful$waiting,breaks="SCOTT")

#PACOTE LATTICE
#library(lattice)
#lattice::histogram()

## Suavização por Loess ##
x <- cars$speed
y <- cars$dist
plot(x,y)
lines(loess.smooth(x,y),lty=2,lwd=2,col="red")

## Suavização por núcleo estimador ##
plot(x,y)
#linha de regressao
abline(lm(y~x),lwd=2,lty=3)
lines(ksmooth(x,y,kernel="n",bandwidth = 2),lwd=2,col="red")

#### Estimação da densidade por núcleo estimador ####
x <- c(0,1,1.1,1.5,1.9,2.8,2.8,3.5)
n<- length(x)
xgrid <- seq(min(x)-1, max(x)+1,by=0.01) 
#h é a janela
for(h in c(0.4,0.1,0.8,2)){
  #aplicacao do nucleo (gaussiano) ao grid
  cont <- sapply(x, function(a) dnorm((xgrid-a)/h)/(n*h))
  #grafico da estimacao e das contribuicoes
  plot(xgrid,rowSums(cont),lwd=2,xlab="x",ylab=expression(hat(f)(x)), type="l")
}

## faithful
x<- faithful$waiting
n<- length(x)
xgrid <- seq(min(x)-1, max(x)+1,by=0.01) 
for(h in c(0.4,0.1,0.8,2)){
  #aplicacao do nucleo ao grid
  cont <- sapply(x, function(a) dnorm((xgrid-a)/h)/(n*h))
  #grafico da estimacao e das contribuicoes
  plot(xgrid,rowSums(cont),lwd=2,xlab="x",ylab=expression(hat(f)(x)), type="l")
}

## BIBLIOTECA KS  ##
#IRIS - Nucleo estimador densidade (univariada)
library(ks)
fhat<- kde(x=iris[,3])
plot(fhat,cont=50,col.cont="blue",cont.lwd=2,
     ylab=expression(hat(f)(x)),xlab="Comprimento de pétala (cm)")

## usando FAITHFUL
fhat<- kde(x=faithful$waiting)
plot(fhat,cont=50,col.cont="blue",cont.lwd=2,
     ylab=expression(hat(f)(x)),xlab="Comprimento de pétala (cm)")
#na munheca:
h<- fhat$h
cont <- sapply(x, function(a) dnorm((xgrid-a)/h)/(n*h))
lines(xgrid,rowSums(cont),lwd=2,lty=2,col="red",
        xlab="x",ylab=expression(hat(f)(x)))

# Nucleo estimador de densidade bivariada
fhat <- kde(x=iris[,3:4])
plot(fhat, display= "filled.contour", cont= seq(10,90,by=10))
plot(fhat, display="persp", thin=3, border=1,col="white",theta=10)

fhat <- kde(x=iris[,2:4])
plot(fhat,drawpoints=T)
