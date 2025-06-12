#Normal multivariada

mux <- c(-1,0,1)
  
sigmax <- matrix(data=c(9,4,2,4,8,3,2,3,7),nrow = 3,byrow = T)

eigen(sigmax)

normais <- rnorm(3)

decomposta <- chol(sigmax)

V.meio <- diag(1/sqrt(diag(sigmax)))

correl.matrix <- V.meio %*% sigmax %*% V.meio

geranormalmultivariada <- function(n,covmat,medias)
{
  dim <- length(mux)
  A <- t(chol(covmat))
  
  dados <- replicate(n,(A%*%rnorm(dim)) + medias)
  
  resposta <- matrix(data=dados,byrow=T,ncol = dim)
  
  return(resposta)
}

valoresgerados <- geranormalmultivariada(100,sigmax,mux)

apply(valoresgerados,2,ks.test,y=pnorm,mean=mux,sd=sqrt(diag(sigmax)))
    
cov(valoresgerados)

colMeans(valoresgerados)

plot(valoresgerados[,1],valoresgerados[,2])

plot(valoresgerados[,1],valoresgerados[,3])

plot(valoresgerados[,2],valoresgerados[,3])

# Usa os scores para fazer uma qui quadrado com 3 graus

xcentrado <- scale(valoresgerados,center=T,scale=F)

inversa <- solve(cov(valoresgerados))

scores <- diag(xcentrado%*%inversa%*%t(xcentrado))

# Q-Q plot dessa qui quadrado

probs <- ppoints(length(scores))

Q.amostra <- sort(scores)

Q.teorico <- qchisq(probs,df=3)

plot(Q.teorico,Q.amostra)

pontoquantilicosx <- qchisq(c(0.25,0.75),3)

pontoquantilicosy <- quantile(Q.amostra)

coeficienteangular <- (pontoquantilicosy[4] - pontoquantilicosy[2])/diff(pontoquantilicosx)

abline(a=0,b=coeficienteangular,lwd=2,col="red")

ks.test(scores,pchisq,df=3)
