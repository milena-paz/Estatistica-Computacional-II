set.seed(666)
amostra <- rnorm(10)

probs <- (rank(amostra)-0.5)/length(amostra)

sort(probs)

qamostra <- amostra

q.teorico <- qnorm(probs,mean(qamostra),sd=sd(qamostra))

plot(x=q.teorico,y=qamostra,main="Q-Qplot Normal")

lines(x=qnorm(c(0.25,0.75),mean(qamostra),sd=sd(qamostra)),y=quantile(qamostra,c(0.25,0.75)))

set.seed(666)
amostra.exp <- rexp(5000)

probs.exp <- (rank(amostra.exp)-0.5)/length(amostra.exp)

sort(probs.exp)

qamostra.exp <- amostra.exp

q.teorico.exp <- qexp(probs.exp,1/mean(amostra.exp))

plot(x=q.teorico.exp,y=qamostra.exp,main="Q-Qplot exp")

lines(x=qexp(c(0.25,0.75),1/mean(amostra.exp)),y=quantile(qamostra.exp,c(0.25,0.75)))

inclinacao.exp <- diff(quantile(qamostra.exp,c(0.25,0.75)))/diff(qexp(c(0.25,0.75),1/mean(amostra.exp)))

abline(0,inclinacao.exp)



plot(0,type="n",xlim=c(0,5),ylim=c(0,1.32))
curve(modulo,from=0,to=5,add=T,lwd=2)
curve(exp(-x),from=0,to=5,add=T,lwd=2)
curve(1.3154*exp(-x),from=0,to=5,add=T,col="red",lwd=2)
abline(v=1)
abline(h=modulo(1))

rexp(1000,1)

set.seed(666)

modulo <- function(x)
{
  return(2*exp(-x^2/2)/sqrt(2*pi))
}



aceitacaorejeicao <- function(M=1.3154,ponto,Unif,func1=modulo,func2=dexp,...)
{
  return(Unif*func1(ponto)<=M*func2(ponto,...))
}

dummy <- F
Unif1 <- 0

pontos <- numeric(10000)


for(i in 1:100)
{
  dummy <- F
  while(!dummy)
  {
    Unif1 <- runif(1)
    ponto.random <- runif(1)
    dummy <- aceitacaorejeicao(ponto=ponto.random,Unif = Unif1,func2 = dbeta,M=2.4576,shape1=2,shape2=5)
  }
pontos[i] <- ponto.random
}

hist(pontos,freq=F,breaks=50,ylim=c(0,1.35))


curve(modulo,from=0,to=8,add=T,lwd=2)
curve(1.3154*exp(-x),from=0,to=8,add=T,col="red",lwd=2)
abline(v=1)
abline(h=modulo(1))

U2 <- runif(1000)