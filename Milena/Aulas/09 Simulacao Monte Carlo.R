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
## qte de componentes redundantes para q P{Funcionar} > 0.99

#### O GATO E O RATO ####
# Suponha que você tenha um timer e uma fila de 5 caixas, com um gato na 1ª caixa
# e um rato na 5ª caixa, no instante inicial. O gato e o rato pulam ambos ao acaso
# para uma caixa adjascente quando o timer avança.
# O gato come o rato se eles estiverem na mesma caixa, terminando o jogo.

#Matriz de transicao
P <- matrix(0,nrow=5,ncol=5)
P[c(2,4),c(3,5)] <- 0.5
P[1,3] <- P[5,5] <- 1
P[3,-3] <- 0.25

estados<- c("(1,5)"=1,"(1,3)"=2,"(2,4)"=3,"(3,5)"=4,"FIM"=5)
atual <- estados[1]

simula <- function(atual,P){
  caminho <- atual
  while(atual!=5){
    atual <-sample(estados,size=1,prob=P[atual,])
    caminho <- append(caminho, atual)
  }#fim while
  return(caminho)
}#fim simula

Sim<-simula(atual, P)

#plota o caminho
plot(x=1:length(Sim),y=Sim,type="b",axes=F)
axis(1,1:length(Sim))
axis(2,at=1:5,labels=names(estados))

S <- replicate(100,simula(atual,P))

#media do tempo do trajeto
mean(sapply(S, length))
#prop de vezes em cada casa
#em cada simulacao
sapply(S,function(sim)table(sim)/length(sim))
#no geral
table(unlist(S))/sum(sapply(S, length))
#tempo maximo
max(sapply(S,length))

#desenha todos os caminhos das simulacoes
plot(x=0,y=0,ylim=c(1,5),xlim=c(1,max(sapply(S,length))),
     xlab="tempo",ylab="estado",
     type="n",axes=F)
axis(1,1:18)
axis(2,at=1:5,labels=names(estados))
sapply(S,function(sim){
    x<- 1:length(sim) + rnorm(1,sd=.2)
    y<- sim# + rnorm(1,sd=.1)
    lines(x,y,col="#00000060")
  }
)

#### FUNCAO DE JOGO DO ARTHUR
pos<- function(x){
  if(x==1) return(x+1)
  if(x==5) return(x-1)
  mov <- sample(c(-1,1),1,prob=c(.5,.5))
  x<- x+mov
  return(x)
}

jogo <- function(rato=1,gato=5){
  while(gato!=rato){
    gato<- pos(gato)
    rato<- pos(rato)
    cat(paste(rato,",",gato,"\n"))
  }
  cat("Rato morreu!")
}
jogo()
