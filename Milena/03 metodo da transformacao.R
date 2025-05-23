#CONVOLUCOES* continuaçao
set.seed(666)
Z.star <- replicate(1E3,sum(runif(12)-0.5))

x<- sample(1:1000,size=500)
y<- seq(1,1000,1)[!(1:1000 %in% x)]

matriz <- matrix(c(Z.star[x],Z.star[y]),nrow=500)
MDist<-apply(matriz**2,1,FUN=sum)
all.equal(diag(matriz%*%t(matriz)), MDist)
#o esperado é 0.5
mean(MDist <= qchisq(p=0.5,df=2))
#.95
mean(MDist <= qchisq(p=0.95,df=2))
#.99
mean(MDist <= qchisq(p=0.99,df=2))
## TRANSFORMAÇÃO DE BOX-MULLER ----
#U1 E U2 IID U1 ~ Unif(0,1)
#entao se: Z1 = sqrt(-2*log(U1))*cos(2*pi*U2)  e Z2= sqrt(-2*log(U1))*sin(2*pi*U2)
#Z1 e Z2 são normais padrão N(0,1)

##provando
#Z1^2 + Z2^2 = -2*log(U1)
#portanto U1= exp( (-1/2)*(Z1^2+Z2^2) )
#Z2/Z1=tg(2*pi*U2) -> U2=arctg(Z2/Z1)/(2*pi)

############ CALCULAR JACOBIANO E TERMINAR A TRANSFORMACAO, ENCONTRANDO AS MARGINAIS
##1a ALTERNATIVA ----
#APLICANDO BOX-MULLER NAS UNIFORMES
set.seed(109)
U1<- runif(1E3)
U2<- runif(1E3)
a<- sqrt(-2*log(U1))
b<- 2*pi*U2
Z1 <- a*cos(b)
Z2 <- a*sin(b)

ks.test(Z1,pnorm)
ks.test(Z2,pnorm)

matriz <- matrix(c(Z1,Z2),ncol=2)
MDist<-apply(matriz**2,1,FUN=sum)

for (p in c(0.5,.95,.99)) print(mean(MDist <= qchisq(p=p,df=2)))

##NORMAL PADRAO -> CAUCHY ----
#Z1/Z2
Cauchy <- Z.star[x]/Z.star[y]
#METODO DA INVERSA
Cauchy2 <- tan(pi*(U1-1/2))

hist(Cauchy2, freq=F, xlim=c(-500,500),breaks=100)

##2a ALTERNATIVA P/ GERAR PAR DE NORMAIS INDEPENDENTES ----
#1- Gerar U1 e U2 independentes uniformes(0,1)
#2- fazer V1 = 2*U1-1 e V2 =2*U2 -1, V1,V2iid uniformes(-1,1)
#3- S= V1² + V2², S~Unif(0,1)
### SE S>1, RETORNAR AO PASSO 1
#4- Z1 = sqrt(-2*log(S)/S)*V1 e Z2 = sqrt(-2*log(S)/S)*V2
##  temos que Z1 e Z2 são normais independentes quando V1,V2
##  é um ponto aleatõrio no circulo unitario
set.seed(666)
S<- 2
while(S>1){
  U <- runif(2)
  V <- 2*U-1
  S <- sum(V**2)
}
Z1<- sqrt(-2*log(S)/S)*V[1]
Z2 <- sqrt(-2*log(S)/S)*V[2]

gera <- function(){
  S<- 2
  cont<-0
  while(S>1){
    U <- runif(2)
    V <- 2*U-1
    S <- sum(V**2)
    cont<- cont+1
  }#fim loop
  Z<- sqrt(-2*log(S)/S)*V
  return(c("Z"=Z,"Tentativas"=cont))
} # fim funcao 
set.seed(666)
X<- replicate(1E3,gera())
1/mean(X[3,]) #deve se aproximar de pi/4
### histograma das Z geradas e a densidade normal padrao
hist(X[-3,],freq=F)
curve(dnorm,add=T,col="blue",lty=2,lwd=2)
ks.test(X[-3,],pnorm)
qqnorm(X[-3,])
qqline(X[-3,],col="blue",lwd=2)


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