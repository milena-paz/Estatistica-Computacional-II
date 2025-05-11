#Geradores congruenciais

#definamos:
#X_n+1=(A*X_n+B)%%m   
#Un= X_n/m          
# | X_0: valor inicial
# | A,B: numeros "grandes"

# m=10 ; A=10 ; B=17 ; X_0=2
X_p <- function(X_n) (103*X_n+17)%%10
X_p(5)

geradora <- function(n,X_0){
  X<- numeric(n)
  X[1]<- X_0
  for(i in 2:n){
    X_0 <- X_p(X_0)
    X[i] <- X_0
  }
  return(X/10)
}

v <- geradora(20, 2)
plot(v,type="b")

# m=2^31 ; A= 65539 ; B=0; X_0=2
X_p2 <- function(X_n) (65539*X_n)%%2^31
geradora2 <- function(n,X_i){
  X<- numeric(n)
  X[1]<- X_i
  for(i in 2:n){
    X_i <- X_p2(X_i)
    X[i] <- X_i
  }
  return(X/(2^31))
}

v<-geradora2(1E5,2)
sum(duplicated(v))
boxplot(v)

X0 <- runif(1E4)
obj<- hist(X0,freq=T,breaks=100)
obj$counts

# m=2^32 ; A=1664525 ; B= 1013904223 ; X_0=2

X_p3 <- function(X_n) (1664525*X_n+1013904223)%%2^32
geradora3 <- function(n,X_i){
  X<- numeric(n)
  X[1]<- X_i
  for(i in 2:n){
    X_i <- X_p3(X_i)
    X[i] <- X_i
  }
  return(X/(2^32))
}
v1 <- geradora2(1E4,2)
v2<-geradora3(1E4,2)

#runif
set.seed(666)
v3 <- runif(1E4)
obj1<- hist(v1,breaks=100)
obj2<- hist(v2,breaks=100)
obj3 <- hist(v3,breaks=100)

c1 <- obj1$counts
c2<- obj2$counts
c3 <- obj3$counts
boxplot(c1,c2,c3)

v1 <- geradora2(2E4,2)
v2<-geradora3(2E4,2)

## AMOSTRA RUNIF ----
set.seed(666)
v3 <- runif(2E4)
#c3 <- hist(v3,breaks=100)$counts

plot(x=v3[seq(1,1E4,2)],y=v3[seq(2,1E4,2)],pch=".")
abline(h=seq(0,1,.1), v=seq(0,1,0.1),col="grey65")
cutPar <- cut(v3[(1:1E4)*2],breaks=0:10/10,include.lowest = T)
cutImp <- cut(v3[(1:1E4)*2-1],breaks=0:10/10,include.lowest = T)
tabela <- table(cutImp,cutPar)

## Grafico com o numero de pontos em cada casela
plot(x=v3[seq(1,1E4,2)],y=v3[seq(2,1E4,2)],pch=".")
pos<-seq(0.05,1,0.1)
for(i in 1:10){
  for(j in 1:10){
    text(pos[i],pos[j],labels=tabela[i,j],cex=0.85,font=2)
  }
}
abline(h=seq(0,1,.1), v=seq(0,1,0.1),col="grey65")
##

boxplot(tabela)

#####-------------------------------------------#####
#Teste de hipótese com a tabela de contingencia
#H0: variaveis independentes
#H1: as variaveis se relacionam de alguma forma
#estatistica de teste qui quadrado: 
#X2 = (soma de todas as caselas - valor esperado de cada casela)^2/valor esperado de cada casela
####--------------------------------------------#####

X2 <- sum((tabela-100)**2/100)
#pvalor
pchisq(107.22, df=99)
#p=0.73, portanto nao se rejeita H0

#funcao de distribuicao acumulada empirica
set.seed(666)
vet2 <- runif(1E3)
empirica <- ecdf(vet2)
plot(empirica,verticals=T)
curve(punif,from=0,to=1,add=T,lwd=2,col="blue")

#teste de komolgorov-smirnov
# com vet2
ks.test(vet2,"punif")
##

set.seed(666)
vet3 <- runif(50)
empirica3 <- ecdf(vet3)
plot(empirica3,verticals=T)
curve(punif,from=0,to=1,add=T,lwd=2,col="blue")

ks<-ks.test(vet3,punif)
ks$p.value

############
# -repetir 1000 vezes a geracao de amostras de n=50 valores
# da uniforme (das 3 funcoes geradoras) e fazer um histograma
# de densidade dos resultados
# -cores no grid quadrado
############

# cores no grid usando hexbin ----
library(hexbin)
plot(hexbin(v3[(1:1E4)*2-1],v3[(1:1E4)*2],xbins=10,ybnds=c(0,1),xbnds=c(0,1)),
     xlab="Índices Ímpares",ylab="Índices Pares")

# cores no grid usando R base ----
par(mar=c(3,3,1,1))
pos<-seq(0,1,0.1)
plot(0,type="n",xlim=0:1,ylim=0:1,xlab="",ylab="",asp=1)
for(i in 1:10){
  for(j in 1:10){
    Cor<-paste0("grey",140-tabela[i,j])
    rect(pos[i],pos[j],pos[i]+.1,pos[j]+.1, col=Cor)
  }
}
points(x=v3[seq(1,1E4,2)],y=v3[seq(2,1E4,2)],pch=".")
pos<-seq(0.05,1,0.1)
for(i in 1:10){
  for(j in 1:10){
    text(pos[i],pos[j],labels=tabela[i,j],cex=0.85,font=2,col="white")
  }
}
abline(h=seq(0,1,.1), v=seq(0,1,0.1),col="grey65")


# cores no grid usando paleta não-cinza ----
#png(filename="colorido.png", width=600,height=600)
paleta<-paletteer_c("grDevices::Blues",40,direction=-1)
#gambiarra sem noção abaixo
ordem <- matrix(as.numeric(factor(tabela)),nrow=10)
#ctrl-c ctrl-v
par(mar=c(3,3,1,1))
pos<-seq(0,1,0.1)
plot(0,type="n",xlim=0:1,ylim=0:1,xlab="",ylab="",asp=1)
for(i in 1:10){
  for(j in 1:10){
    Cor<- paleta[teste[i,j]] #exceto isso
    rect(pos[i],pos[j],pos[i]+.1,pos[j]+.1, col=Cor)
  }
}
points(x=v3[seq(1,1E4,2)],y=v3[seq(2,1E4,2)],pch=".")
pos<-seq(0.05,1,0.1)
for(i in 1:10){
  for(j in 1:10){
    text(pos[i],pos[j],labels=tabela[i,j],cex=0.85,font=2)
  }
}
abline(h=seq(0,1,.1), v=seq(0,1,0.1),col="grey30")
#dev.off()