#### MISTURAS ####
# A v.a. X satisfaz o modelo de mistura de distribuição independentes se sua fdp
#(ou funcao de massa) satisfaz a equacao:
# f_X = sum( p_i*f_Yi )
# onde sum(p_i) = 1, e todo 0 <= p_i <= 1


### EXEMPLO A: MISTURA DE NORMAIS
#Y1 ~ N(0,1)
#Y2 ~ N(3/2,1/4)
# X = p*Y1 + (1-p)*Y2

p <- 3/4
#temos que a densidade de X é dada por
dXmist <- function(x){
  p*dnorm(x,0,1) + (1-p)*dnorm(x,1.5,0.5)
}

curve(dXmist(x),from=-5,to=5)
curve(p*dnorm(x,0,1),add=T,lty=2,col="orange")
curve((1-p)*dnorm(x,1.5,0.5),add=T,lty=3,col="blue")

pXmist <- function(x) 
  p*pnorm(x,0,1) + (1-p)*pnorm(x,1.5,0.5)

#GERANDO ESSA VA X
X<- numeric(1E3)
U <- runif(1E3)
caso1 <- U <= p
X[caso1] <- rnorm(sum(caso1),0,1)
X[!caso1] <- rnorm(sum(!caso1),1.5,0.5)

hist(X,freq=F,breaks=30)
curve(dXmist(x),add=T)

mean(X)

plot(ecdf(X),lwd=2)
curve(pXmist,add=T,col="blue",lty=2,lwd=2)

ks.test(X,pXmist)

###EXEMPLO B
# X = p1*Y1 + p2*Y2 + p3*Y3
p2 <- p1 <- 9/20
p3<- 1 - p1 - p2
#onde Y1 ~ N(0,1), Y2 ~ N(-6/5,16/25) e Y3 ~N(0,1/4)
mu <- c(0,-1.2,0)
sigma <- c(1,0.8,0.5)

dXmist <- function(x){
  p1*dnorm(x,mu[1],sigma[1]) + p2*dnorm(x,mu[2],sigma[2]) + p3*dnorm(x,mu[3],sigma[3])
}

curve(dXmist, lwd=2,from=-5,to=5)
curve(p1*dnorm(x,mu[1],sigma[1]),add=T,col="maroon",lty=2)
curve(p2*dnorm(x,mu[2],sigma[2]),add=T,col="blue",lty=2)
curve(p3*dnorm(x,mu[3],sigma[3]),add=T,col="darkgreen",lty=2)

pXmist <- function(x) 
  p1*pnorm(x,mu[1],sigma[1]) + p2*pnorm(x,mu[2],sigma[2]) + p3*pnorm(x,mu[3],sigma[3])

#GERANDO ESSA VA X
X<- numeric(1E3)
U <- runif(1E3)
caso1 <- U <= p1
caso2 <- U <= p1+p2 & U > p1
caso3 <- U > p1+p2
X[caso1] <- rnorm(sum(caso1),mu[1],sigma[1])
X[caso2] <- rnorm(sum(caso2),mu[2],sigma[2])
X[caso3] <- rnorm(sum(caso3), mu[3],sigma[3])

hist(X,freq=F,breaks=30)
curve(dXmist(x),add=T)

mean(X)

plot(ecdf(X),lwd=2)
curve(pXmist,add=T,col="blue",lty=2,lwd=2)

###EXEMPLO C
#X= soma de 8 Yk ~ N(3*((2/3)^k - 1), (2/3)^2k) (de k=0 a k=7) com probabilidades p=1/8
fYi <- function(x,k){
  a <- (2/3)^k
  return(dnorm(x,3*(a-1), a))
}


densX <- function(x){
  somas <- rowSums(sapply(0:8,function(k) fYi(x,k) ))
  return(somas/8)
}
curve(densX(x), from=-5, to=5,lwd=2)

FYi <- function(x,k){
  a <- (2/3)^k
  return(pnorm(x,3*(a-1), a))
}
pX <- function(x){
  soma <- rowSums(sapply(0:8,function(k) fYi(x,k) ))
  return(soma/8)
}

#### CRIAR UMA FUNCAO DENSIDADE, ACUMULADA E GERADORA PARA QUALQUER MISTURA DE NORMAIS
##DENSIDADE
dMisturaNorm <- function(x,mi,dp,probs){
  n <- length(mi)
  stopifnot(!any(sapply(c(mi,dp,probs),is.na)),
            all.equal(n,length(dp),
            length(probs)),
            sum(probs)==1,
            all(dp>0))
  ##
  if(length(x)==1)
    d <- sum(probs*dnorm(x,mi,dp))
  else
    d <- rowSums(sapply(1:n,function(k) probs[k]*dnorm(x,mi[k],dp[k])))
  return(d)
}

###APLICANDO NOS EXEMPLOS ANTERIORES

#EXEMPLO A
#Y1 ~ N(0,1)
#Y2 ~ N(3/2,1/4)
# X = p*Y1 + (1-p)*Y2
p <- c(0.75,0.25)
mu<- c(0,1.5)
sigma <- c(1,0.5)

curve(dMisturaNorm(x,mu,sigma,p),from=-5,to=5,lwd=3,col="darkorange")
#comparando com o metodo feito antes
##
p <- 3/4
dXmist <- function(x){
  p*dnorm(x,0,1) + (1-p)*dnorm(x,1.5,0.5)
}
curve(dXmist(x),col="blue",lwd=3,lty=2,add=T)
##

#EXEMPLO B
#onde Y1 ~ N(0,1), Y2 ~ N(-6/5,16/25) e Y3 ~N(0,1/4)
p<- c(9/20,9/20,0.1)
mu <- c(0,-1.2,0)
sigma <- c(1,0.8,0.5)

curve(dMisturaNorm(x,mu,sigma,p),from=-5,to=5,lwd=3,col="darkorange")
#comparando com o metodo feito antes
##
dXmist <- function(x){
  p[1]*dnorm(x,mu[1],sigma[1]) + p[2]*dnorm(x,mu[2],sigma[2]) + p[3]*dnorm(x,mu[3],sigma[3])
}
curve(dXmist(x),col="blue",lwd=3,lty=2,add=T)
##

### ACUMULADA
pMisturaNorm <- function(x,mi,dp,probs){
  n <- length(mi)
  stopifnot(!any(sapply(c(x,mi,dp,probs),is.na)),
            all.equal(n,length(dp),
                      length(probs)),
            sum(probs)==1,
            all(dp>0))
  if(length(x)==1)
    d <- sum(probs*pnorm(x,mi,dp))
  else
    d <- rowSums(sapply(1:n,function(k) probs[k]*pnorm(x,mi[k],dp[k])))
  return(d)
}

###GERADORA ALEATORIA
rMisturaNorm <- function(n,mu,dp,probs){
  k<- length(mu)
  stopifnot(!any(sapply(c(n,mu,dp,probs),is.na)),all.equal(k,length(dp),length(probs)),sum(probs)==1,all(dp>0))
  X<- numeric(n)
  #caso especial, quando nao é uma mistura
  #--------------------------------------#
  if(k==1){
      X <- rnorm(n,mu,dp);return(X)
    }
  #--------------------------------------#
  U <- runif(n)
  caso <- list(0)
  #extremidades
  p <- cumsum(probs)
  caso[[1]] <- U <= p[1]
  caso[[k]] <- U > p[k-1]
  if(k==2){
    tam <- sapply(caso,sum)
    X[ caso[[1]] ] <- rnorm(tam[1],mu[1],dp[1])
    X[ caso[[2]] ] <- rnorm(tam[2],mu[2],dp[2])
    return(X)
  }
  #intermediarios
  for(i in 2:(k-1))
   caso[[i]] <- U<= p[i] & U> p[i-1]
  tam <- sapply(caso,sum)
  for(i in 1:k){
    X[ caso[[i]] ] <- rnorm(tam[i],mu[i],dp[i])
  }
  return(X)
}
##### testando a geradora
p<- c(9/20,9/20,0.1)
mu <- c(0,-1.2,0)
sigma <- c(1,0.8,0.5)
#
teste <- rMisturaNorm(1E3,mu,sigma,p)
#
hist(teste,freq=F,breaks=30)
curve(dMisturaNorm(x,mu,sigma,p),add=T,lwd=3,col="#007BFF")
#
plot(ecdf(teste),lwd=3,col="darkred")
curve(pMisturaNorm(x,mu,sigma,p),add=T,col="#007BFF",lwd=3,lty=3)
#
ks.test(teste,"pMisturaNorm",mi=mu,dp=sigma,probs=p)