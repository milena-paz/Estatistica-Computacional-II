## INTEGRAÇÃO MONTE CARLO
#obj: resolução de integrais definidas usando números aleatórios.


## * Integrais do tipo theta= integral (calculada em [0,1]) de g(x)dx ----
#   Seja U~Unif(0,1):
#   theta= E(g(U)) = integral(de 0 a 1) de g(x)*f_U(x)*dx; onde f_U(x) = 1
#   Assim, temos o seguinte algoritmo para estimar theta:

# 1- Gerar n variáveis U(0,1) aleatoriamente
# 2- Aplicar g(U)
# 3- Calcular a média das g(U)

## ESTIMAR B(0.4,5) = integral de 0 a 1 de t^(x-1)*(1-t)^(y-1)*dt
theta <- beta(0.4,5)

g<- function(t, x=0.4,y=5){
  t^(x-1) * (1-t)^(y-1)
}

estima<- function(n){
  U <- runif(n)
  return(mean(g(U)))
}

n<- c(100,500,1E3,5E3,1E4,5E4,1E5,1E6)
valores <- sapply(n, estima)

plot(log(n,10),valores)
abline(h=theta,col="blue",lwd=2,lty=2)
text(4,1.20,labels= bquote(theta == .(theta)),col="blue")

## realizando 100 aproximacoes
aprox <- t(replicate(100, sapply(n,estima)))

plot(log10(n), y=1:8, ylim=c(0.4,3.6),type="n")
for(i in 1:100){
  points(log10(n),aprox[i,])
}
abline(h=theta,col="blue",lwd=2,lty=2)

#medias das estimativas
medias <- apply(aprox, 2, mean)
names(medias) <- n
medias

#erro padrao das estimativas
ep <- apply(aprox,2,sd)
names(ep) <- n
ep

#quantis (limite superior e inferior)
Limites <-apply(aprox,2, function(x) quantile(x,probs=c(0.025,0.975)))

#amplitudes
amplitude <- apply(aprox,2, function(x) diff(range(x)))

#tabela compilando tudo
cbind(n,medias,ep,LInf=Limites[1,],LSup=Limites[2,],amplitude)

## 24/06
## * INTEGRAÇAO NO INTERVALO (a, b) ----
# - Realizamos a transformação 
# X = Y(b-a) + a
# Y = (X - a) / (b-a)
# dY = 1/(b-a) dX
# - Substitui-se na integral de modo que os limites de integração se tornem (0,1)
# - Volta à linha 10 da nota de aula

### EXEMPLO
# Seja X~N(0,1)
# Deseja-se calcular por integração MC: theta = P( 1 < X < 2 )

#g(x)= dnorm(x,mean=0,sd=1)

U <- runif(1E3)
t <- mean( dnorm(U+1) )
# VALOR REAL:
theta <- pnorm(2) - pnorm(1)
rbind(estimativa=t,valor.real=theta)

estima<- function(n){
  U <- runif(n)
  return(mean(dnorm(U+1)))
}
aprox <- t(replicate(100, sapply(n,estima)))

plot(log10(n), y=1:8, ylim=c(0.10,0.15),type="n")
for(i in 1:8){
  points(rep(log10(n)[i],100),aprox[,i])
}
abline(h=theta,col="blue",lwd=2,lty=2)
text(5,0.14,labels= bquote(theta == .(theta)),col="blue")

# violin plot das estimativas
library(vioplot)
colnames(aprox) <- n
vioplot(aprox, col="lightgreen")

# medias das estimativas
medias <- apply(aprox, 2, mean)
names(medias) <- n
medias

#erro padrao das estimativas
ep <- apply(aprox,2,sd)
names(ep) <- n
ep


## * INTEGRAIS NO INTERVALO (0,Inf) ----
# 1- Substituição Y = 1/(X-1):
#   X = 1/y - 1
#   quando X-> 0, Y==1
#   quando X-> Inf, Y==0
#   Jacobiano: J(y)= -1/y^2
# 2- Volta à linha 10 da nota de aula


### EXEMPLO
# theta = Integral do 0 ao Inf de exp(-x^2)
# Valor Real: 
theta <- sqrt(pi)/2

h <- function(y){
  exp(-(1/y - 1)^2)/y^2
}
#estimando
U <- runif(1E3)
est <- mean(h(U))

c(estimativa=est,valor.real=theta)

estima<- function(n){
  U <- runif(n)
  return(mean(h(U)))
}
n<- c(1E3,5E3,1E4,1E5)
X<- t(replicate(1E3, sapply(n,estima)))
amp <- range(X)
#media das estimativas para cada n
medias <- round(apply(X,2,mean),3)
#desvio padrao das estimativas para cada n
dp <- round(apply(X,2,sd),3)

par(mfrow=c(2,2),mar=c(2,4.1,2,1))
for(i in 1:4){
  hist(X[,i],xlim=amp,freq=F,breaks=50,ylim=c(0,220),
       border="gray30",
       main=paste0("n=",n[i]))
  abline(v=theta,col="blue",lwd=2,lty=2)
  lines(density(X[,i]), col="darkgreen",lwd=2.5)
  text(x=.94,y=210,bquote(theta == .(round(theta,3))))
  text(x=.94,y=190,bquote(bar(X) == .(medias[i])))
  text(x=.94,y=170,paste("ep =", dp[i]))
}