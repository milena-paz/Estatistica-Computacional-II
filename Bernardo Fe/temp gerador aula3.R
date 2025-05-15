pontos <- geradornormalbivariada(1000,12,intervalo=c(-0.5,0.5))

#Mostrando que a distancia de mahalanobis^2 é menor ou 
#igual o quantil da chi quadrado.

matriz.pontos <- matrix(c(pontos$x,pontos$y),nrow=500,ncol=2,byrow = T)

distancias <- apply(matriz.pontos^2,MARGIN = 1,FUN=sum)

mean(distancias <= qchisq(p=0.5,df=2))

mean(diag(matriz.pontos %*% t(matriz.pontos)) == distancias)

#Novo método de gerar normais, transformacao de box muller1
#Vou trocar ela pela geradornormalbivariada() na 'funcoes rng.R'

geradornovobivariada <- function(n)
{
  primeiro <- runif(n)
  
  segundo <- runif(n)
  
  valores <- list(x=sqrt(-2*log(primeiro))*cos(2*pi*segundo),
                  y=sqrt(-2*log(primeiro))*sin(2*pi*segundo))
  
  return(valores)
}

pontos2 <- geradornovobivariada(500)

matriz.pontos2 <- matrix(c(pontos2$x,pontos2$y),nrow=500,ncol=2,byrow = T)

distancias2 <- apply(matriz.pontos2^2,MARGIN = 1,FUN=sum)

mean(distancias2 <= qchisq(p=0.5,df=2))

mean(distancias2 <= qchisq(p=0.95,df=2))
     
mean(distancias2 <= qchisq(p=0.99,df=2))

mean(diag(matriz.pontos2 %*% t(matriz.pontos2)) == distancias2)

#teste de k-smirnoff

ks.test(pontos2$x,punif)

ks.test(pontos$y,punif)

#Mesma coisa das chi quadrados la em cima

mean(distancias <= qchisq(p=0.5,df=2))

mean(distancias <= qchisq(p=0.95,df=2))

mean(distancias <= qchisq(p=0.99,df=2))

mean(distancias2 <= qchisq(p=0.5,df=2))

mean(distancias2 <= qchisq(p=0.95,df=2))

mean(distancias2 <= qchisq(p=0.99,df=2))

#fazer uma cauchy

pontos3 <- geradornovobivariada(500)

pontos4 <- pontos2$x/pontos3$x

#histograma do pontos4 (que é uma cauchy)

hist(pontos4,freq=F,xlim=c(-500,500))
#minimo e maximo de pontos4
max(pontos4)
min(pontos4)
