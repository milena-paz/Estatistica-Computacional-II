discretauniforme <- function(n,alcance)
{
  return(trunc(runif(n,alcance[1],alcance[2]+1)))
}

questaolista <- function(n,qtd)
{
  exp(n/qtd)
}

gerados <- discretauniforme(1e3,c(1,1e4))

mean(questaolista(gerados,1e4)*(1e4))

integral_mcm <- function(n,g,a=0,b=1) {mean(g(runif(n,a,b))*(b-a))}

integral_mcm(1e5,dnorm,a=1,b=2)

qtes <- c(1e2,5e2,1e3,5e3,1e4,5e4,1e5,1e6)

cem <- replicate(100,sapply(qtes,integral_mcm,g=dnorm,a=1,b=2))

rowMeans(cem)

plot(x=log(qtes,base=10),y=c(1:8),ylim = c(0.1,0.15),type="n")

for(i in 1:8)
{
  points(x = rep(log(qtes[i],base=10),100),y=cem[i,])
}

abline(h=diff(pnorm(c(1,2))),lty=2,lwd=2,col="blue")
