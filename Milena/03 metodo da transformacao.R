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
