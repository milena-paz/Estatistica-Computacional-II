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

#gera pontos dentro de um quadrado -1<=x<=1 , -1<=y<=1
#guarda e conta quando x^2+y^2<=1
geradorcirculo <- function(n=1)
{
  #qtd é o contador pra guardar os pontos no vetor z1 e z2
  qtd <- 0
  #qtd2 é o contador de vezes que o código rodou até gerar os n pontos
  qtd2 <- 0
  #criando z1 e z2
  z1 <- numeric(n)
  z2 <- numeric(n)
  while(qtd<n)
  {
    v1 <- runif(1,-1,1)
    v2 <- runif(1,-1,1)
    s <- v1^2 + v2^2
    if(s<1)
    {
      #nesse if o qtd só aumenta quando o s for menor que 1, e então
      #guarda os pontos da normal em z1 e z2 na posição "qtd" (contador)
      qtd <- qtd + 1
      z1[qtd] <- sqrt(-2*log(s)/s)*v1
      z2[qtd] <- sqrt(-2*log(s)/s)*v2
    }
    #qtd2 conta quantas vezes esse while rodou, caso s for maior q um, esse contador aumenta
    #mas o qtd não, já o while só para quando o qtd chegar em n
    qtd2 <- qtd2 + 1
  }
  print(paste("Esse código rodou",qtd2,"vezes"))
  print(paste("Mas encontrou",n,"pontos"))
  print(paste("A proporção é:",n/qtd2))
  #lista com os pontos da normal, a quantidade de vezes que rodou, e a proporção
  #do total com a quantidade
  resposta <- list(pontos=matrix(c(z1,z2),nrow=n,byrow = F),quantidade = qtd2,proporcao = n/qtd2)
  return(resposta)
}

aceitacaorejeicao <- function(M=1.3154,ponto,f=modulo,g=dexp,...)
{
  return(runif(1)<=f(ponto,...)/(M*g(ponto)))
}

gerador.aceitacao <- function(n,maximo,f_x,g_y,rg_y,...)
{
  dummy <- F
  Uni <- 0
  pontos <- numeric(n)
  contador <- 0
  
  for(i in 1:n)
  {
    dummy <- F
    while(!dummy)
    {
      ponto.random <- rg_y(1)
      dummy <- aceitacaorejeicao(M=maximo,f=f_x,g=g_y,ponto=ponto.random,...)
      contador <- contador + 1
    }
    pontos[i] <- ponto.random
  }
  print(paste("o codigo rodou",contador,"vezes"))
  print(paste("mas gerou",n,"valores"))
  print(paste("taxa de aproveitamente de ",round(100*n/contador,digits=3),"%",sep = ""))
  return(pontos)
}

geradorbernoulli <- function(n,p)
{
  return(as.numeric(runif(n)<p))
}

geradorbinomial <- function(k,n,p)
{
  replicate(k,sum(geradorbernoulli(n,p)))
}

geradorgeometrica <- function(n,p)
{
  return(log(runif(n))%/%log(1-p) + 1)
}

graficodebarras <- function(valoresgerados,distribuicao,cores=c("#aa00aa99","#ffaa3399","#ff0838"),...)
{
  tabela<- table(valoresgerados)/length(valoresgerados)
  nomes <- names(tabela)
  
  #gambiarra forte, é só para criar a escala do barplot
  barplot(tabela,
          names.arg=nomes,col="#ffffff00",border=F
          ,main="densidade dos valores gerados",ylim=c(0,max(tabela)+0.1))
  grid(col=cores[3])
  barplot(distribuicao((as.numeric(nomes))-1,...),
          names.arg=nomes,col=cores[1],add=T,border = F)
  barplot(tabela,
          names.arg=nomes,col=cores[2],border=F,add=T)
  legend("topright",legend = c("proporcao teórica","proporcao empírica"),col=cores,lwd=5)
}

dnormal.pvariada <- function(x,cov.mat,medias)
{
  exp(-t(x-medias)*cov.mat*(x-medias))/((2*pi)^(length(medias)/2)*(det(cov.mat))^(1/2))
}

gerabivariadadependente <- function(n,medias,sigma,rho)
{
  valoresx2 <- rnorm(n,mean=medias[2],sd=sqrt(sigma[2]))
  
  valoresx1 <- rnorm(n,mean=(medias[1]+
                               rho*sqrt(sigma[1]/sigma[2])*(valoresx2-medias[2])),sd=sqrt(sigma[1]*(1-(rho)^2)))
  
  pontos <- matrix(c(valoresx1,valoresx2),nrow=n,byrow = F)
  
  print(apply(pontos,2,mean))
  
  print(cov(pontos))
  
  plot(pontos)
  
  return(pontos)
}
