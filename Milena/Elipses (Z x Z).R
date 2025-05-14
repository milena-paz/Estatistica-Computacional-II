set.seed(666)
Z.star <- replicate(1E3,sum(runif(12)-0.5))

#fazer aquele grafico dos pontinhos
#desenhar no plano onde esta 50, 95, 99% da densidade de Z.star com elipses 
x<- sample(1:1000,size=500)
y<- seq(1,1000,1)[!(1:1000 %in% x)]
plot(Z.star[x],Z.star[y],pch=19)
#nesse caso, serão circunferências já que Z.star[x] e Z.star[y] 
#são identicamente distribuídas
#X=R*cos(T) , Y= R*sin(T)
cartesiano <- function(r,t){
  list(x= r*cos(t), y= r*sin(t))
}

#Usando os resultados do Bernardo, a funcao de distribuicao acumulada
#de R é: F(r)= 1 - exp(-(r^2)/2)
#além disso, a funcao quantilica é:
qR <- function(p){
  return( sqrt(-2*log(1-p)) )
}

draw.circle <- function(raio,cor="black"){
  theta <- seq(0,2*pi,0.05*pi)
  coords <- cartesiano(r=raio,t=theta)
  lines(coords$x,coords$y, col=cor,lwd=2)
}
#p/ 50%:
qR(.5) |> draw.circle(cor="blue")
#p/ 95%:
qR(.95) |> draw.circle(cor="orange")
#p/ 99%:
qR(.99) |> draw.circle(cor="magenta")