#Boot strap

amostra.original <- c(3,5,6,7,9)

set.seed(666)

amostra.boot <- sample(amostra.original,5,replace=T)

mean(amostra.boot)

mean(replicate(1e3,mean(sample(amostra.original,5,replace=T))))

library(gtools)

amostras.boot <- permutations(n=5,r=5,v=amostra.original,repeats.allowed = T)

medias.boot <- apply(amostras.boot,1,mean)

medias.bootstrap <- replicate(1e3,mean(sample(amostra.original,5,replace=T)))

hist(medias.bootstrap,freq=F)


#Correção de viés

set.seed(666)

n <- 25

x <- rnorm(n)

head(x)

theta <- 1

theta.hat <- function(x) var(x)*(n-1)/n

sigma2.hat <- theta.hat(x)

c(sig2.amost=theta.hat(x),erro.amost=theta.hat(x)-theta,bias.esp=-1/n)

N <- 5000

vetor <- replicate(N,theta.hat(sample(x,n,replace=T)))

theta.star <- mean(vetor)

c(theta.star,theta.star-theta.hat(x))

install.packages("bootstrap")

#usar bootstrap para ver o erro padrão entre a correlação LSAT,GPA

library(bootstrap)

set.seed(666)

cor.boot <-function(){
  ind <- sample(1:nrow(law),15,replace=T)
  return(with(law[ind,],cor(LSAT,GPA)))
}

correlacoes <- replicate(1e3,cor.boot())

sd(correlacoes)

hist(correlacoes,freq=F)

r <- function(x,i) cor(x[i,1],x[i,2])

boot(law,r,R=2e3)
