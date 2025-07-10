#gato e rato, tem uma fila de 5 caixas, com um gato na primeira e um rato na quinta
#o gato e orato podem ao acaso se mudar para a casa adjacente
#se eles se encontram o gato come o rato

jogo <- matrix(c(0,0,1,0,0,0,0,1/2,0,0.5,0.25,0.25,0,0.25,0.25,0,0,0.5,0,0.5,0,0,0,0,1),
               nrow = 5,byrow = T)

sample(c(1:5),size=1,prob=jogo[5,])

chegada <- function()
{
  estado.atual <- 1
  contador <- numeric(5)
  while(estado.atual!=5)
  {
    estado.atual <- sample(c(1:5),size=1,prob=jogo[estado.atual,])
    contador[estado.atual] <- contador[estado.atual] + 1
  }
 return(contador) 
}
chegada()

jogo.gato.rato <- function(n)
{
  valores <- replicate(n,chegada())
  print("Média de tentativas até acabar:")
  print(mean(apply(valores,2,sum)))
  print("Proporções de vezes visitadas em cada casa:")
  print(apply(valores,1,mean))
}
jogo.gato.rato(1e3)

#detalhe: eu não considero o estado inicial gato no 1 e rato no 5 como um movimento.


#simulando e modelando de um modo mais arcaico

rademacher <- function()
  {return(2*rbinom(1,1,0.5)-1)}

ajuste <- function(x) {return(abs(abs(x-5)-4)+1)}

jogo.arcaico <- function()
{
  gato <- 1
  rato <- 5
  contador <- 0
  while(gato!=rato)
  {
    gato <- ajuste(gato + rademacher())
    rato <- ajuste(rato + rademacher())
    print(paste("gato:",gato,"rato:",rato))
    contador <- contador + 1
  }
  return(contador)
}

jogo.arcaico()

mean(replicate(1e4,jogo.arcaico()))
