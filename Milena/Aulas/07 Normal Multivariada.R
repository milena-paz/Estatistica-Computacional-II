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
Multinorm <- function(n,mu1,mu2,var1,var2,corr){
  var.str <- var1*(1-corr^2)
  X2 <- rnorm(n, mu2,sqrt(var2))
  mu.str <- mu1 + corr*sqrt(var1/var2)*(X2-mu2)
  X1 <- sapply(mu.str, function(mu){
    rnorm(1,mu,sqrt(var.str))
  })
  return(matrix(c(X1,X2), ncol=2))
}
X.til <- Multinorm(100,mu1,mu2,var1,var2,corr)
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
M <- scale(X.til,center=T,scale=F) %*% solve(S) %*% t(scale(X.til,center=T,scale=F))
scores <- diag(M)

ks.test(scores, pchisq, df=100)
probs <- (rank(scores)-0.5)/100
plot(scores, qchisq(probs,df=100))