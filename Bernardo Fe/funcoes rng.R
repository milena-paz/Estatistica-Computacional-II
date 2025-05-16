gerador <- function(n,A=321,B=656,m=727) (A*n+B)%%m

geradorunif <- function(n,inic,mod,...)
{
  x <- numeric(n)
  x[1] <- inic
  for(i in 2:n)
    x[i] <- gerador(n=x[i-1],m=mod,...)
  return(x/mod)
}

#Forma os pares para o grafico
pareador <- function(vet)
{
  pares <- seq(2,length(vet),2)
  impares <- pares - 1
  return(list(y=vet[pares],x=vet[impares]))
}

#Nome fraco pra caramba, nao consegui pensar em algo melhor :/
#plota a tabela de 10x10 quadrados e escreve la dentro quantos tem
plotar.tabela <- function(pontos,...)
{
  plot(pontos$x,pontos$y,pch=".",...)
  
  abline(h=c(0:10)/10,v=c(0:10)/10,col="#555555")
  
  tabela <- table(cut(pontos$x,breaks=10,include.lowest = T),cut(pontos$y,breaks=10,include.lowest = T))
  
  for(lin in 1:10)
  {
    for(col in 1:10)
    {
      text(x=(lin-0.5)/10,y=(col-0.5)/10,labels=tabela[lin,col],font=2)
    }
  }
}

#compara a acumulada empirica de alguns valores com a acum. teorica da uniforme
acumuladaempirica <- function(valores,...)
{
  plot(ecdf(valores),lwd=2,...)
  curve(punif,col="red",add=T,lwd=2,...)
}

#desenha qualquer curva parametrica dada por funct, onde o parametro vai de range[1] a range[2]
parametric.draw <- function(range,funct,precision=100,...)
{
  #faz "precision" linhas para aproximar a curva
  points <- seq(from=range[1],to=range[2],length.out=precision)
  coords <- list(x = funct$x(points), y = funct$y(points))
  lines(coords$x,coords$y,...)
}

#Funcao de um circulo q pode ser colocada no parametric.draw
circle <- function(r) list(x = function(t) r*cos(t), y = function(t) r*sin(t))

antigo.geradornormalbivariada <- function(qtd,k,intervalo=c(0,1),...)
{
  valores <- replicate(qtd,sum(runif(k,min=intervalo[1],max=intervalo[2])))
  return(pareador(valores))
}

#funcao quantilica do raio da normal bivariada
raio.quantil <- function(quantil,desvio)
{
  return(desvio*sqrt(-2*log(1-quantil)))
}

#Calcula quantos pontos estao dentro de algum raio especifico (se dividir pela quantidade de pontos voce tem o quantil daquele raio)
quantilempirico <- function(lista,raio)
{
  sum((lista$x^2 + lista$y^2 <= raio^2))
}

#Algoritmo de boxmuller
geradornormalbivariada <- function(n,sigma=1)
{
  primeiro <- runif(n)
  
  segundo <- runif(n)
  
  valores <- list(raio.quantil(primeiro,sigma)*cos(2*pi*segundo),
                  y=raio.quantil(primeiro,sigma)*sin(2*pi*segundo))
  
  return(valores)
}
