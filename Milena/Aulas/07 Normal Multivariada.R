#### Normal Multivariada ####
#para gerar uma normal multivariada precisariamos de:
#- vetor de médias
#- matriz de covariâncias

### Normal Bivariada
#deseja-se gerar 100 pares ordenados de uma normal bivariada
#com
mu1<-1 ; mu2<- 3; var1<- 2; var2<- 1; corr <- -0.8
#para isso:
# -Calcular var.str = var1*(1-corr^2)
# -Geramos X2 de acordo com sua distribuição marginal
# -Calculamos mu.str <- 1 - corr*sqrt(var2/var1)*(X2-3)
# -Gerar X1 de acordo com uma N(mu.str,var.str)
# -Retornar (X1,X2)
BiNorm <- function(n,mu1,mu2,var1,var2,corr){
  var.str <- var1*(1-corr^2)
  X2 <- rnorm(n, mu2,sqrt(var2))
  mu.str <- mu1 + corr*sqrt(var1/var2)*(X2-mu2)
  X1 <- sapply(mu.str, function(mu){
    rnorm(1,mu,sqrt(var.str))
  })
  return(matrix(c(X1,X2), ncol=2))
}
X.til <- BiNorm(1E3,mu1,mu2,var1,var2,corr)
X1 <- X.til[,1]
X2 <- X.til[,2]

plot(X1,X2)

hist(X1,freq=F,breaks=25)
curve(dnorm(x,mu1,sqrt(var1)),add=T,col="blue",lwd=2)
hist(X2, freq=F,breaks=25)
curve(dnorm(x,mu2,sqrt(var2)),add=T,col="red",lwd=2)
#medias
(medias <- apply(X.til,2,mean))
# matriz de covariancias
S <- cov(X.til)
#correlacao
cov(X1,X2)/sqrt(var1*var2)

## Seja Xtil ~ Np(mutil,sigma), entao:
#(xtil-mutil)%*% solve(sigma) %*%(xtil-mutil) ~ Chisq(p) , onde solve() é a inversão da matriz
#entao
#scale(Xtil,center=T,scale=F) %*% solve(S) %*% t(scale(Xtil,center=T,scale=F))
#
Xc<- scale(X.til,center=T,scale=F)

scores <- diag(Xc %*% solve(S) %*% t(Xc))

#construcao do Q-Q plot da amostra
ks.test(scores, pchisq, df=2)

probs <- ppoints(length(scores))
q.amostra<- sort(scores)
q.teorico <- qchisq(probs, df=2)
plot(q.teorico, q.amostra, main="Q-Q plot Qui-Quadrado(2)")
#b<- (q.amostra[75]-q.amostra[25])/(q.teorico[75]-q.teorico[25])
#abline(a=q.amostra[25] - q.teorico[25]*b,b= b,
#         col="blue", lty=2,lwd=3)
qqline(q.amostra, distribution = function(p) qchisq(p,df=2))

#### Normal multivariada por Decomposicao de Cholesky ####
#temos Z1,Z2,...,Zp iid Z~N(0,1)
#aij, i, j =1,...,p]
# Xtil= A*Ztil + mitil
# e Xtil ~ N(mitil,SigmaX)

# OBJETIVO: Gerar vetor de p nũmeros aleatõrios de uma normal multivariada com 
#   vetor de médias mitil
#   matriz de covariancias SigmaX
mi<- c(-1,0,1)
SigmaX <- matrix(c(9,4,2,4,8,3,2,3,7),ncol=3)
# 1- GERAR P NUMEROS ALEATORIOS DE NORMAL PADRAO
Z <- rnorm(3)
# 2- Encontrar uma matriz A t.q. SigmaX = A*t(A) (decomposicao de Cholesky)
A <- t(chol(SigmaX))
# 3- Calcular X = A*Z+mi
X<- A%*%Z+mi
###
V.meio<- diag(1/sqrt(diag(SigmaX)))
V.meio%*%SigmaX%*%V.meio #matriz de correlacao teorica

geraMultinorm <- function(n, p, mi, sigma){
  Z<- replicate(n, rnorm(p))
  #podemos mudar essa geracao da normal padrao usando box-muller
  A <- t(chol(SigmaX))
  X <- apply(Z,2,function(z) A%*%z+mi )
  return(t(X))
}

X<- geraMultinorm(100,3,mi,SigmaX)
S <- cov(X) # matriz de covariancas amostral
colMeans(X) # media amostral
R<- cor(X) # matriz de correlacao amostral

#verificar normalidade das marginais
sapply(1:3, function(i){
  ks.test(X[,i], pnorm, mean=mi[i], sd= sqrt(SigmaX[i,i]))
})

# Visualizando as distribuicoes bivariadas:
#(X1,X2); (X2,X3) ; (X1,X3)
plot(X[,1],X[,2])
plot(X[,2],X[,3])
plot(X[,1],X[,3])

## Q-Q plot da normal trivariada
Xc<- scale(X,center=T,scale=F)

scores <- diag(Xc %*% solve(S) %*% t(Xc))

#construcao do Q-Q plot da amostra

ks.test(scores, pchisq, df=3)

probs <- ppoints(length(scores))
q.amostra<- sort(scores)
q.teorico <- qchisq(probs, df=3)
plot(q.teorico, q.amostra, main="Q-Q plot Qui-Quadrado(3)")
qqline(q.amostra, distribution = function(p) qchisq(p,df=3),
       col="blue", lty=2,lwd=3)