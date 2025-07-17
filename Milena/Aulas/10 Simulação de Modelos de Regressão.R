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


#### REGRESSAO LINEAR MÚLTIPLA ####
# variáveis explicativas: X e Ia
# Y = B0 + B1*X + B2*Ia + e
# e ~ N(0,sigma^2)
###
# Ia: indicadora do grupo A
#se grupo=A:
# Y = B0 + B1*X + B2*1 + e
#   = B0 + B2 + B1*X + e
#se grupo != A
# Y = B0 + B1*X + e
###
#teste de que se essa regressao e significativa ou nao
## HIPOTESES:
##Ho : B1=...=Bp=0
##Ha : pelo menos um beta é diferente de 0

#### EXEMPLO ----
#Y = 5 - 2*X + 2*Ia
n<- 50
# X ~ N(0,1)
X <- rnorm(n)
#1/2 GRUPO A
#1/2 NAO GRUPO A
grupo <- factor(rep(c("A","B"),each=n/2),levels=c("B","A"))
# e ~ N(0,4)
e <- rnorm(n,sd=2)
Y <- 5 - 2*X + 2*(as.numeric(grupo)-1) + e
#modelo linear
ml <- lm(Y~X+grupo)
cbind(ml$coefficients,summary(ml)$sigma)

plot(X,Y,col=as.numeric(grupo),pch=19)

ggfortify::autoplot(ml)

#RESPONDENDO AO TESTE:
#Ho: a estatistica f ~ F(2,47)
# Mas simulamos Ha...
#
#estatistica F:
est <-summary(ml)$f
#p-valor
1-pf(est[1],df1=est[2],df2=est[3])
with(ml,{
  abline(coefficients[1],coefficients[2],lty=2)
  abline(coefficients[1]+coefficients[3],coefficients[2],col=2,lty=2)
  }
)

estimaParametros <- function(){
  e <- rnorm(n,sd=2)
  Y <- 5 - 2*X + 2*(as.numeric(grupo)-1) + e
  pares <- data.frame(Y,X)
  ajuste <- lm(Y~X+grupo,data=pares)
  estat <- summary(ajuste)$f
  pvalor <- 1-pf(estat[1],estat[2],estat[3])
  return(c(ajuste$coefficients,
           sigma=summary(ajuste)$sigma,
           pvalor))
}

param <- t(replicate(1000,estimaParametros()))
nomes <- c(expression(beta[0]==5),
           expression(beta[1]==-2),
           expression(beta[2]==2),
           expression(sigma==2))

sum(param[,5] > 0.05)

for(i in 1:4){
  hist(param[,i],freq=F,col="salmon",main= nomes[i])
  lines(density(param[,i]),lty=2,lwd=2)
}
#media, desvio padrao e intervalo de confianca das estimativas
rbind(Media=apply(param,2,mean),
      DesvioPadrao=apply(param,2,sd),
      apply(param,2,quantile,probs=c(0.025,0.975))
)

estimaParametros <- function(){
  e <- rnorm(n,sd=2)
  Y <- 5 + e
  pares <- data.frame(Y,X)
  ajuste <- lm(Y~X+grupo,data=pares)
  estat <- summary(ajuste)$f
  pvalor <- 1-pf(estat[1],estat[2],estat[3])
  return(c(ajuste$coefficients,
           sigma=summary(ajuste)$sigma,
           pvalor))
}

param <- t(replicate(1000,estimaParametros()))
sum(param[,5] > 0.05)

hist(param[,5],freq=F,col="salmon",main="p-valor")
