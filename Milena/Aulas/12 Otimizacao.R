#### OTIMIZACAO ####
#(MAXIMIZACAO OU MINIMIZACAO DE FUNCOES)


## exemplos de algoritmo (CORTEZ, Modern optimization with R ,2014)
# Busca exaustiva: envolve buscar por todo o espaço de busca
# Busca determinística: hillclimbing, tabu...
# Simulated annealing
# Algoritmo genético
# Colona de formigas
# Nuvem de partículas

## EXEMPLO 1
# Suponha que queiramos achar o máximo da função 
# f(x) = exp(-(x-2)^2)
#FUNCAO OPTIM()
#por padrao ela minimiza, entao pode-se usar -f(x) para maximizar
#de forma:

f <- function(x) -exp(-(x-2)^2)
#metodo padrao (Nelder-Mead)
optim(1,fn=f)
#metodo brent
optim(1,fn=f, method="Brent",lower=0,upper=4)
#metodo CG (usa variada)
df <- function(x) -2*(x-2)*f(x)
optim(par=1,fn=f,gr=df,method="CG")
# O método Nelder-Mead é mais sensível ao valor inicial, mas não usa derivada.

## EXEMPLO 2
#f(x) = sin(cos(x)*x)
f <- function(x) sin(cos(x)*x)
curve(f(x),0,10)
suppressWarnings(
(param <- sapply(c(2,4,6,7,8,9),function(i) optim(i,fn=f)$par))
)
f(param)

## EXEMPLO 2
#f(x) =  log(1+log(x))/log(1+x)
f <- function(x) -log(x+log(x))/log(1+x)
curve(-f(x), from= 0.3, to=50)
-f(2)
-f(5.792299)
optim(2,fn=f)
### NO CASO UNIVARIADO, PODEMOS USAR A FUNCAO optimize{stats} (ou optimise)
optimise(f,interval=c(2,10))


## EXEMPLO 3
#funcao de Rosenbrock
f <- function(x, y) (1 - x)^2 + 100*(y - x^2)^2

x <- seq(-2, 2, by = 0.15)
y <- seq(-1, 3, by = 0.15)
z <- outer(x, y, f)
library(rgl)
plot3d(f, xlim=c(-2,2),ylim=c(-1,3),col=viridis::viridis_pal(option="C"))
persp(x,y,z,phi = 45, theta = -45, col = "orange", shade = 0.00000001, ticktype = "detailed")
contour(x,y,z=z,col="brown",lwd=2,nlevels=10)

## EXEMPLO 4
f <- function(x,y) 0.6*exp(-((x-2)^2+(y-4)^2)/2)+0.25*exp(-((x-5)^2+(y-2)^2)/2)+0.15*exp(-((x-3)^2+(y-6)^2)/2)

x <- seq(0, 8, by = 0.1)
y <- seq(0, 8, by = 0.1)
z <- outer(x, y, f)
persp(x,y,z,phi = 15, theta = 60, col = "orange", shade = 0.00000001, ticktype = "detailed",lwd=0.4)

plot3d(f, xlim=c(0,8),ylim=c(0,8),col=viridis::viridis_pal(option="C"))
contour(x,y,z,col="brown",lwd=2,nlevels=10)

f <- function(xy){
  dif <- xy-c(2,4,5,2,3,6)
  return(-sum(c(0.6,0.25,0.15)*exp(-(dif[1:3*2]^2+dif[1:3*2-1]^2)/2)))
}#fim f "vetorizada"
#
#vet <- c(2,4)
#dif <- vet-c(2,4,5,2,3,6)
#-sum(c(0.6,0.25,0.15)*exp(-(dif[1:3*2]^2+dif[1:3*2-1]^2)/2))
maximo <-optim(par=c(3,3),fn=f) # resulta num maximo global
optim(par=c(5,4),fn=f) # resulta num maximo local

#metodo SANN
estimativas<- -replicate(100,optim(par=c(6,4),fn=f,method="SANN")$value)
hist(estimativas,col="orange",freq=F,breaks=25)
mean(estimativas<0.615 & estimativas > 0.605)
##
estimativas<- -replicate(100,optim(par=c(7,4),fn=f,method="SANN")$value)
hist(estimativas,col="orange",freq=F,breaks=25)

###
#conjunto de dados cars{datasets}
#pretende-se encontrar um modelo linear
# dist.hat = beta0 + beta1*speed
#CRITERIO p/ ESTIMAR BETA0 E BETA1
#F.ob = sum((dist-dist.hat)^2)
#     = sum((dist - beta0 - beta1*speed)^2)

#por soma de quadrado dos desvios
f <- function(vet){
  b0 <- vet[1]
  b1 <- vet[2]
  return(sum((cars$dist-b0-b1*cars$speed)^2))
}
estima <-optim(par=c(0,0),fn=f)$par
plot(dist ~ speed, data=cars)
abline(a=estima[1],b=estima[2],lwd=2)
ml<-lm(dist ~ speed, data=cars)
abline(ml,col="red",lty=2)

# desvio absoluto
f <- function(vet){
  b0 <- vet[1]
  b1 <- vet[2]
  return(sum(abs(cars$dist-b0-b1*cars$speed)))
}
estima <-optim(par=c(0,0),fn=f)$par
abline(a=estima[1],b=estima[2],lwd=1.5,col="blue",lty=3)

# minimos quadrados de segundo grau
f <- function(vet){
  b0 <- vet[1]
  b1 <- vet[2]
  b2 <- vet[3]
  return(sum((cars$dist-b0-b1*cars$speed-b2*cars$speed^2)^2))
}
estima <-optim(par=c(0,0,0),fn=f)$par