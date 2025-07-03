# O problema se parece com uma ponte de wheatstone, porém são chaves em todos os pontos.
# E queremos ver a probabilidade do sistema funcionar dado as probabilidades de cada chave funcionar.


#matriz de adjacencias

M <- matrix(c(1,1,0,0,1,1,0,0,0,0,0,1,0,1,0,0,1,1,0,0,1,0,0,0,1),byrow=T,nrow = 5)

M_2 <- M%*%M

M_3 <- M%*%M_2

#Vetor de posicao inicial

V <- c(1,0,0,0,0)

Simulador <- function(P)
{
  if(length(P)==1) P <- rep(P,5)
  
  Chave <- sapply(P,function(probs){sample(c(1,0),size=1,replace=F,prob = c(probs,1-probs))})
  
  Mat_Trans <- matrix(c(0,Chave[1],Chave[4],0,0,0,Chave[3],Chave[2],0,Chave[3],
                        0,Chave[5],0,0,0,1),byrow=T,nrow=4)
  
  M_2 <- Mat_Trans %*% Mat_Trans
  
  M_3 <-  Mat_Trans %*% M_2 
  
  return(M_3[1,4]>=1)
}

mean(replicate(1e3,Simulador(c(0.9,0.9,0.9,0.9,0.9))))

Calcula_Valor_Real <- function(P)
{
  real <- P[1]*P[2] + P[4]*P[3]*P[2]+
          P[1]*P[3]*P[5] + P[4]*P[5]
  
  return(real)
}
