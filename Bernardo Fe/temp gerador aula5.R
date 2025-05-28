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

aceitacaorejeicao <- function(M=1.3154,ponto,U,f=modulo,g=dexp,...)
{
  return(U<=f(ponto)/(M*g(ponto,...)))
}

dummy <- F
Unif1 <- 0

pontos <- numeric(10000)

contador <- 0

for(i in 1:10000)
{
  dummy <- F
  while(!dummy)
  {
    Unif1 <- runif(1)
    ponto.random <- -log(runif(1))
    dummy <- aceitacaorejeicao(ponto=ponto.random,U = Unif1)
    contador <- contador + 1
  }
  pontos[i] <- ponto.random
}
print(paste("o codigo rodou",contador,"vezes"))

hist(pontos,freq=F,breaks=50,ylim=c(0,1.35),xlim=c(0,4))
curve(modulo,from=0,to=8,add=T,lwd=2)
curve(1.3154*exp(-x),from=0,to=8,add=T,col="red",lwd=2)
curve(exp(-x),from=0,to=8,add=T,col="blue",lwd=2)
abline(v=1)
abline(h=modulo(1))
