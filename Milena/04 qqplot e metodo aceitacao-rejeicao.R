# Q-Q PLOT ----
##exemplos exp, cauchy, t de student
amostra<- rexp(100)
qqnorm(amostra)
qqline(amostra,lwd=2,col="blue")

amostra<- rcauchy(100)
qqnorm(amostra)
qqline(amostra, lwd=2,col="blue")

amostra<- rt(100,3)
qqnorm(amostra)
qqline(amostra, lwd=2,col="blue")

set.seed(666)
X <- rnorm(10)
##QQplot usa as estatisticas de ordem de uma amostra e seus quantis
#em relação ao valor teorico (no caso de QQnorm, o quantil normal)
q.amostra <- X
q.teorico <- qnorm(rank(X)/10, mean(X), sd(X))

plot(x=q.teorico,y=q.amostra,main="QQplot normal",xlab="Quantis teóricos",
     ylab="Quantis Amostrais")

#alternativa --
probs <- (rank(X)-0.5)/length(X)
q.teorico<- qnorm(probs, mean(X),sd(X))
plot(x=q.teorico,y=q.amostra,main="QQplot normal",xlab="Quantis teóricos",
     ylab="Quantis Amostrais")
lines(x=qnorm(c(0.25,0.75),mean(X),sd(X)),y=quantile(X,c(0.25,0.75)),col="darkgreen",lwd=2)

#Exponencial
set.seed(666)
X<- rexp(50)
probs<- (rank(X)-0.5)/length(X)
q.amostra<-X
##estimador do parametro da exponencial: lambdachapeu = 1/Xbarra
q.teorico<- qexp(probs, rate=1/mean(X))
plot(x=q.teorico,y=q.amostra,main="QQplot exponencial",xlab="Quantis teóricos",
     ylab="Quantis Amostrais")
lines(x=qexp(c(0.25,0.75),1/mean(X)),y=quantile(X,c(0.25,0.75)),col="blue",lwd=2)

## METODO DA ACEITAÇÃO-REJEIÇÃO

dens1<- function(x){
  men<- x<0
  dens <- numeric(length(x))
  dens[men]<- 0
  dens[!men]<- 2 * exp(-x**2/2) / sqrt(2*pi)
  return(dens)
}
dens2<- function(y){
  men<- y<0
  dens <- numeric(length(y))
  dens[men]<- 0
  dens[!men]<- exp(-y)
  return(dens)
}
#na razao fX/fY, o máximo foi p/ x=1, onde: 
M<-sqrt(2)*exp(1/2)/sqrt(pi)
#é a razao maxima

curve(M*dens2(x),from=0,to=5,col="blue",lwd=2)
curve(dens1(x),from=0,to=5,lwd=2,add=T)

#ALGORITMO:
#1: GERAR Y ~ exp(1)
gera <- function(){
  Y<- 0
  U<- 20
  cont<- 0
  while(M*U>dens1(Y)/dens2(Y)){
    U <- runif(1)
    V <- runif(1)
    X <- -log(U)
    Y <- V*exp(-X)*M
    cont<- cont+1
  }
  return(c(Y,cont))
}
Y<-replicate(1E4,gera())
hist(Y,freq=F,breaks=20)

mean(Y[2,])

#GERAR UMA Y~Beta(2,5)
#moda dessa beta seria 1/5=0.2
curve(dbeta(x,2,5),from=0,to=1)
points(0.2,dbeta(0.2,shape1=2,shape2=5),pch=19)
M<- dbeta(0.2,shape1=2,shape2=5)
#taxa de aceitacao = 1/M
#gy: uniforme
#meio fraca essa taxa...

#### EXEMPLO 2.2 CASELLA (2010) pg. 62/297(46) ----
#Sejam Xi's V.A's iid Exponencial(1), entao tres distribuições padrão podem
#ser obtidas por essas mesmas.
# Y1= 2*sum(Xi,de: i=1, até: r) ~ Chisq(2*r)
# Y2= beta*sum(Xi,de: i=1, até: alfa) ~ Gama(alfa,beta)
# Y3= sum(Xi,de: i=1, até: alfa)/sum(Xi,de: i=1, até: alfa+beta) ~ Beta(alfa,beta)
####

#U ~ Unif(0,1)
#Fx ~ beta(2.7,6.3)
#Gy ~ beta(2,6)
#Aceita-se U <= f(Y)/(M*g(y))

#deseja-se maximizar a razao fX(y)/gX(y):
razao <- function(x){
  dbeta(x, 2.7,6.3)/dbeta(x,2,6)
}

#ALTERNATIVA PARA DERIVACAO E MAXIMIZACAO À MÃO
M<-optimise(razao,maximum=T,interval=c(0,1))$objective

curve(M*dbeta(x,2,6),col='blue',lwd=2 )
curve(dbeta(x, 2.7,6.3),add=T,lwd=2)
legend(x="topright",legend=c("M*g","f"),col=c("blue","black"),lwd=2)

## METODO 1 - gerando um monte e verificando quais sao aceitas
n<- 2500
set.seed(666)
MU<- M*runif(n)
Y<- rbeta(n,2,6)
aceita <- MU<= razao(Y)
X <- Y[aceita]
taxa <- mean(aceita)
###
hist(X,freq=F)
curve(dbeta(x, 2.7,6.3),add=T,lwd=2,col="red")

### GRAFICO ACEITACAO REJEICAO
curve(M*dbeta(x,2,6),col='blue',lwd=2 )
curve(dbeta(x, 2.7,6.3),add=T,lwd=2)
#pontos aceitos
ordenadas<- MU*dbeta(Y,2,6)
points(X, ordenadas[aceita],cex=0.85)
#pontos rejeitados
points(Y[!aceita],ordenadas[!aceita],cex=0.85,col='blue')


## METODO 2 - gerando e validando com while
gera<- function(){
  U<- 10
  Y<- 0.1
  while(M*U>razao(Y)){
    U<- runif(1)
    Y<- rbeta(1,2,6)
  }
  return(Y)
}

X<- replicate(1E4,gera())
hist(X, freq=F,col="grey90",breaks=20)
curve(dbeta(x, 2.7,6.3),add=T,lwd=2,col="red")


## O "tempo" T  entre eventos numa poisson é uma V.A exponencial
#que nao tem memoria, assim como a geometrica(que e discreta)
#Se U~Unif(0,1)
#T =  -log(U)/l ~ exp(l)


#prox aula geracao de discreta?