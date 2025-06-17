## INTEGRAÇÃO MONTE CARLO
#obj: resolução de integrais definidas usando números aleatórios.


# * Integrais do tipo theta= integral (calculada em [0,1]) de g(x)dx
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