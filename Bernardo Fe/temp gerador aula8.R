p1 <- 1/2
mi1 <- 0
mi2 <- 2
sigma1 <- 1
sigma2 <- 1

dxmist <- function(x)
{
  p1*dnorm(x,mean=mi1,sd=sigma1)+(1-p1)*dnorm(x,mean=mi2,sd=sigma2)
}

dxmist(1)

curve(dxmist(x),from=-3,to=5,lwd=3,add=T)
curve(p1*dnorm(x,mean=mi1,sd=sigma1),col="red",add=T,lwd=2)
curve((1-p1)*dnorm(x,mean=mi2,sd=sigma2),col="blue",lwd=2,add=T)

pxmist <- function(x)
{
  p1*pnorm(x,mean=mi1,sd=sigma1)+(1-p1)*pnorm(x,mean=mi2,sd=sigma2)
}

geraxmist <- function(x)
{
  condicao <- x < p1
  y <- numeric(length(x))
  y[condicao] <- rnorm(sum(condicao),mean=mi1,sd=sigma1)
  y[!condicao] <- rnorm(sum(!condicao),mean=mi2,sd=sigma2)
  return(y)
}

hist(geraxmist(runif(1e5)),freq=F,breaks=20)
curve(dxmist(x),from=-3,to=5,lwd=3,add=T)
curve(p1*dnorm(x,mean=mi1,sd=sigma1),col="red",add=T,lwd=2)
curve((1-p1)*dnorm(x,mean=mi2,sd=sigma2),col="blue",lwd=2,add=T)

plot(ecdf(geraxmist(runif(1e3))))
curve(pxmist(x),from=-5,to=5,lwd=2,col="red",add=T,lty=2)

ks.test(geraxmist(runif(1e3)),pxmist)

p1 = 9/20
p2 = 9/20

mi1 = 0
mi2=-6/5
mi3=0

sigma1 <- 1
sigma2 <- 4/5
sigma3 <- 1/2

geraymist <- function(x)
{
  condicao1 <- x < p1
  condicao2 <- x < p1 + p2
  y <- numeric(length(x))
  y[(condicao1)] <- rnorm(sum(condicao1),mean=mi1,sd=sigma1)
  y[(condicao2) & (!condicao1)] <- rnorm(sum((condicao2) & (!condicao1)),mean=mi2,sd=sigma2)
  y[(!condicao2) & (!condicao1)] <- rnorm(sum((!condicao2) & (!condicao1)),mean=mi3,sd=sigma3)
  return(y)
}

dymist <- function(x)
{
  p1*dnorm(x,mean=mi1,sd=sigma1)+(p2)*dnorm(x,mean=mi2,sd=sigma2) + (1-p1-p2)*dnorm(x,mean=mi3,sd=sigma3)
}

pymist <- function(x)
{
  p1*pnorm(x,mean=mi1,sd=sigma1)+(p2)*pnorm(x,mean=mi2,sd=sigma2)+(1-p1-p2)*pnorm(x,mi3,sigma3)
}

hist(geraymist(runif(1e5)),freq = F,breaks=30)
curve(dymist(x),from=-4,to=4,add=T,col="red",lwd=2)

plot(ecdf(geraymist(runif(1e3))))
curve(pymist(x),from=-5,to=5,lwd=2,col="red",add=T,lty=2)


dzmist <- function(x,medias,sigmas,p)
{
  valor <- 0
  for(k in length(p))
  {
    valor <- valor + p[k]*dnorm(x,medias[k],sigmas[k])
  }
  return(valor)
}

curve(dzmist(x,3*((2/3)^(0:7)-1),(2/3)^(2*c(0:7)),rep(1/8,8)),from=-2.85,to=-2.8,lwd=2)
