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

acumuladaempirica <- function(valores,...)
{
  plot(ecdf(valores),lwd=2,...)
  curve(punif,col="red",add=T,lwd=2,...)
}

parametric.draw <- function(range,funct,precision=100,...)
{
  points <- seq(from=range[1],to=range[2],length.out=precision)
  coords <- list(x = funct$x(points), y = funct$y(points))
  lines(coords$x,coords$y,...)
}

circle <- function(r) list(x = function(t) r*cos(t), y = function(t) r*sin(t))

antigo.geradornormalbivariada <- function(qtd,k,intervalo=c(0,1),...)
{
  valores <- replicate(qtd,sum(runif(k,min=intervalo[1],max=intervalo[2])))
  return(pareador(valores))
}

raio.quantil <- function(quantil,desvio)
{
  return(desvio*sqrt(-2*log(1-quantil)))
}

quantilempirico <- function(lista,raio)
{
  sum((lista$x^2 + lista$y^2 <= raio^2))
}

geradornormalbivariada <- function(n)
{
  primeiro <- runif(n)
  
  segundo <- runif(n)
  
  valores <- list(x=sqrt(-2*log(primeiro))*cos(2*pi*segundo),
                  y=sqrt(-2*log(primeiro))*sin(2*pi*segundo))
  
  return(valores)
}
