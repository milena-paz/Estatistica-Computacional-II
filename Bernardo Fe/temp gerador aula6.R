aceitacaorejeicao <- function(M=1.3154,ponto,...)
{
  return(runif(1)<=dbeta(ponto,2.7,6.3)/(M*dbeta(ponto,2,6)))
}

beta <- function(x,alfa,beta) dbeta(x,shape1=alfa,shape2=beta)

razao <- function(x)
{
  beta(x,2.7,6.3)/beta(x,2,6)
}

Max <- optimize(f=razao,interval=c(0,1),maximum = T)

dummy <- F
Unif1 <- 0

pontos <- numeric(100000)

contador <- 0

for(i in 1:100000)
{
  dummy <- F
  while(!dummy)
  {
    ponto.random <- rbeta(1,shape1=2,shape2=6)
    dummy <- aceitacaorejeicao(M=Max$objective,ponto=ponto.random)
    contador <- contador + 1
  }
  pontos[i] <- ponto.random
}

print(paste("o codigo rodou",contador,"vezes"))

hist(pontos,freq=F,breaks=50,ylim=c(0,3),xlim=c(0,1))
curve(beta(x,2.7,6.3),from=0,to=1,add=T,lwd=2)
curve(Max$objective*beta(x,2,6),from=0,to=1,add=T,col="red",lwd=2)
curve(beta(x,2,6),from=0,to=1,add=T,col="blue",lwd=2)



Ngera <- 2500

Y <- rbeta(Ngera,shape1=2,shape2=6)

Mu <- Max$objective * runif(Ngera)

aceita <- Mu <= razao(Y)

x <- Y[aceita]

mean(aceita)

plot(0,type="n",xlim = c(0,1),ylim=c(0,5))
curve(beta(x,2.7,6.3),from=0,to=1,add=T,lwd=2)
curve(Max$objective*beta(x,2,6),from=0,to=1,add=T,col="red",lwd=2)

points(x=Y[aceita],y=Mu[aceita]*beta(Y[aceita],alfa=2,beta=6),col="blue",pch=21,cex=0.8)

points(x=Y[!aceita],y=Mu[!aceita]*beta(Y[!aceita],alfa=2,beta=6),col= "red",cex=0.8,pch=21)


