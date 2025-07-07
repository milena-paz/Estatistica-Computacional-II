### SIMULAÇÃO DE MONTE CARLO ###
#matriz de adjacências de um grafo
M <- matrix(c(1,1,0,0,1,
              1,0,1,1,0,
              0,0,0,1,0,
              0,0,1,0,0,
              1,0,0,0,1),ncol=5)

#adjacências em 2 steps
M %*% M 
#adjacências em 3 steps
(M %*% M) %*% M
##
#valor exato (para todos os p iguais e p=0.99)
p<- 0.5
2*p^2+2*p^3-5*p^4+2*p^5

#simulando o circuito:
p <- c(0.99,0.99,0.99,0.99,0.99) #probabilidades
C <- sapply(p, function(p) rbinom(1,1,p)) #chaves fechadas ou nao
#chave <- Vectorize(rbinom, vectorize.args="prob")
M <- matrix(c(0,0,0,0,
              C[1],0,C[3],0,
              C[4],C[3],0,0,
              0,C[2],C[5],1),ncol=4)

(M3 <- (M %*% M) %*% M)

simula <- function(p){
  C <- sapply(p, function(p) rbinom(1,1,p))
  M<- matrix(0,nrow=4,ncol=4)
  M[1,2] <- C[1]; M[2,4] <- C[2]
  M[2,3] <- M[3,2] <- C[3]
  M[1,3] <- C[4];  M[3,4] <- C[5];  M[4,4] <- 1
  passos <- M %*% M %*% M
  funciona <- min(passos[1,4], 1)
  return(funciona)
}
Nsim <- 1E3
p <- c(0.99,0.99,0.99,0.99,0.99)
Sim <- replicate(Nsim,simula(p))
mean(Sim)
##
p<- c(.5,.5,.5,.5,.5)
Sim<- replicate(Nsim, simula(p))
mean(Sim)
##
p<- c(0.8,0.8,0.5,0.8,0.8)
Sim <- replicate(Nsim, simula(p))
mean(Sim)
## qte de componentes redundantes para que P{Funcionar} > 0.99

##FUNCAO PARA O CIRCUITO QUE ELE POS NO QUADRO

simula2 <- function(p){
  C <- sapply(p, function(p) rbinom(1,1,p))
  M<- matrix(0,nrow=5,ncol=5)
  M[1,-1] <- c(C[1],C[3],C[5],C[4])
  M[-1,5] <- c(C[2],C[2],C[6],1)
  passos <- M %*% M %*% M
  funciona <- min(passos[1,5], 1)
  return(funciona)
}
#exemplo
# p=1/2 para todas as 6 chaves
p <- rep(0.5,6)
Nsim <- 1E3
Sim<- replicate(Nsim, simula2(p))
#resp teorica:
p <- 0.5; p+3*p**2-4*p**3-p**4+3*p**5-p**6
# resultado simulacao:
mean(Sim)