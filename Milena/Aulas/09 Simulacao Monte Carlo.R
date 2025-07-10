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


#### PROBLEMA DA RUÍNA DO JOGADOR ####
# O jogador tem probabilidade p de ganhar R$1,00 e (1-p) de perder R$1,00.
#Suponha que o jogador iniciou o jogo com i reais.
#A) Assumindo que as sucessíveis jogadas sejam independentes, qual é a probabi-
# lidade de que a fortuna alcance N reais antes de o jogador perder tudo?
#B) Qual é o numero médio de jogadas até chegar a N reais?
#C) Qual é o número médio de jogadas até que ele perca tudo?
#dado:
p<- 0.4
i<-3
N<- 7

simula <- function(p){
  fort <- i
  cont <- 0
  while(fort>0 && fort <N){
    fort <- fort+ (-1)^as.numeric(runif(1)>p)
    cont <- cont+1
  }
  return(c(cont, fort==N))
}

X<- t(replicate(1E3,simula(p)))
#A)
mean(X[,2])
#B)
mean(X[X[,2]==1,1])
#C)
mean(X[X[,2]==0,1])

#Simulacao sem limite superior
simula <- function(p,N){
  fort <- i
  cont <- 0
  #alvo <-0
  while(fort>0){
    fort <- fort+(-1)^as.numeric(runif(1)>p)
    #alvo <- alvo + as.numeric(fort >=N)
    cont <- cont+1
  }
  #return(c(cont,alvo))
  return(cont)
}
#X<- t(replicate(1E3,simula(p,N)))
X <- replicate(1E3,simula(p,N))
mean(X[,2]>0)

#Simulacao em que o apostador joga uma moeda para decidir se aposta denovo ou nao
simulaMoeda <- function(p){
  fort <- i
  cont <- 0
  moeda <-1
  while(fort>0 && moeda>0.5){
    fort <- fort+(-1)^as.numeric(runif(1)>p)
    cont <- cont+1
    moeda <- runif(1)
  }
  return(c(cont,fort))
}
Xmoeda<- t(replicate(100,simulaMoeda(p)))

mean(Xmoeda)
#probabilidade do jogador iniciar em i e atingir 1real antes de chegar a N reais?
# valor real: 0.8795
#probabilidade de começar em 2 e quebrar antes de atingir 6 reais?
#probabilidade de atingir 2 antes de chegar a 4 reais

##Sem jogar uma moeda
simula2 <- function(p,inferior=0,superior,fort){
  cont <- 0
  while(fort>inferior && fort <superior){
    fort <- fort+ (-1)^as.numeric(runif(1)>p)
    cont <- cont+1
  }
  return(c(cont,fort==inferior))
}
# I)
X <- t(replicate(1E3, simula2(p,1,N,i)))
mean(X[,2])
# II)
X <- t(replicate(1E3, simula2(p,superior=6,fort=2)))
mean(X[,2])
# III)
X <- t(replicate(1E3, simula2(p,inferior=2,superior=4,i)))
1-mean(X[,2])

##jogando uma moeda
simula3 <- function(p,inferior=0,superior,fort){
  cont <- 0
  U<-1
  while(fort>inferior && fort <superior && U>0.5){
    fort <- fort+ (-1)^as.numeric(runif(1)>p)
    cont <- cont+1
    U<- runif(1)
  }
  return(c(cont,fort==inferior))
}
# I)
X <- t(replicate(1E3, simula3(p,1,N,i)))
mean(X[,2])
# II)
X <- t(replicate(1E3, simula3(p,superior=6,fort=2)))
mean(X[,2])
# III)
X <- t(replicate(1E3, simula3(p,inferior=2,superior=4,i)))
1-mean(X[,2])
