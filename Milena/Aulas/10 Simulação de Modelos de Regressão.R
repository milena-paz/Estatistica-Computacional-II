## MODELOS DE REGRESSAO
#SIMPLES 
#Y= B0+B1*X+e
# Y: variável resposta
# X: variável explicativa/preditora
# e ~ N(0,sigma²) : erro independente de X
# E(Y|X=x)=E(B0+B1*X+e|X=x)=B0+B1*x+0
# Y|X=x ~ N(B0+B1x,sigma²)

#estima-se três parâmetros desse modelo:
# B0,B1 e sigma^2


###exemplo: 
#simular Y= 150 - 4X + e
# e ~ N(0,10)
# X é controlado entre 20 e 80
# objetivo: obter N=50 pares (Y,X)

# 1- Gerar 50 valores de X
#    podem ser escolhidos de maneira deterministica
#    ou aleatoriamente
# 2- Gerar 50 valores de e
# 3- Calcular Y = 150 - 4X + e
# 4- Montar o data frame Y,X

#X deterministica
X <- seq(from=20,to=80,length.out=50)
#X aleatoria uniforme
X <- runif(50,20,80)
#X aleatoria normal
X <- rnorm(50,50,10)
#gerando epsilon
e <- rnorm(50,sd=sqrt(10))
Y <- 150 - 4*X + e

pares <- cbind(Y,X)
plot(Y ~ X,data=pares)

modeloSim <- function(X){
  n<-length(X)
  e <- rnorm(n,sd=sqrt(10))
  Y <- 150 - 4*X + e
  return(data.frame(Y,X))
}
pares <- modeloSim(100,X)
plot(Y ~ X,data=pares)
#correlação
cor(pares$X,pares$Y)
#desvios padrão
apply(pares,2,sd)
#valor teorico da correlacao:
#r = B1*sd(X)/sd(Y)
-4*sd(pares$X)/sd(pares$Y)

ajuste <-lm(pares$Y~pares$X)

#estimativas de B0 e B1
intercepto <- ajuste$coefficients[1]
inclinacao <- ajuste$coefficients[2]

#erro padrao dos residuos
sd(ajuste$residuals)
#ou
summary(ajuste)$sigma

estimaParametros <- function(X){
  pares <- modeloSim(X)
  #plot(Y~X,data=pares)
  ajuste <- lm(Y~X,data=pares)
  return(c(ajuste$coefficients,
           sigma=summary(ajuste)$sigma))
}
X <- rnorm(50,50,10)
estima <- t(replicate(1E3,estimaParametros(X)))
nomes <- c(expression(beta[0]==150),
           expression(beta[1]==-4),
           expression(sigma==10))

for(i in 1:3){
  hist(estima[,i],freq=F,col="brown1",main= nomes[i])
  lines(density(estima[,i]),lty=2,lwd=2,col="blue")
}
#media, desvio padrao e intervalo de confianca das estimativas
rbind(Media=apply(estima,2,mean),
      DesvioPadrao=apply(estima,2,sd),
      apply(estima,2,quantile,probs=c(0.025,0.975))
)
