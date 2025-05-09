hist(u1+u2,freq = F)
segments(c(0,2),0,1,1,col="red",lwd=2)

#densidade
dUs <- function(x){
  return(  12*(x-0.5)^2  )
}
curve(dUs,from=0,to=1)
#FDA
pUs <- function(x){
  return(  4*(x-0.5)^3 + 0.5  )
}
##funcao quantilica
qUs <- function(p){
  return( sign(p-0.5)*(abs(p/4 - 0.125))^(1/3) + 0.5 )
}
#funcao geradora de numeros aleatorios
rUs<- function(n){
  return( qUs(runif(n)) )
}
set.seed(109)
XUs<- rUs(1E5)
mean(XUs>=0.5)
mean(XUs<0.5)

qUs(c(0.25,0.5,.75))
hist(rUs(1E4), freq=F, breaks=50L)

#teste de hipótese - H0: media=0.5, H1: media!= 0.5

t.test(x=XUs, mu=0.5)
###
#integrando numericamente o nucleo de uma densidade de normal padrao
integrate(function(x) {exp(-x**2/2)},lower=-Inf,upper=Inf)

##### fazer os intervalos de 10% 