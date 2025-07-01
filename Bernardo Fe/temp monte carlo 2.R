#método monte carlo para integrais de 0 a infinito
#y= 1/(x+1), x= 1/y - 1, x=0 => y=1, x=\infty => y=0
#fizemos essa substituição para ter uma integral em termos de y onde
#essa integral vai de 1 a 0,e onde antes tinha f(x) agora tem f(1/y - 1)
#e dx/dy = -1/(y^2) => entao a integral agora é:
# -\int_{1} ^{0} g(1/y -1)/(y^2)dy
#mas como tem um menos ali fora você pode inverter os limites de integração
# e fica: \int_{0}^{1} g(1/y -1)/(y^2)dy
# que é a E[g(y)] de uma uniforme 0 1

#calculando a integral gaussiana de 0 a infinito

gauss <- function(x) exp(-1*x^2)

integral.montecarlo <- function(x,funcao) funcao(1/x -1)/(x^2)

mean(replicate(1e3,(mean(integral.montecarlo(runif(1e3),gauss)))))

grafico <- function()
{
  valores1 <- replicate(1e3,mean(integral.montecarlo(runif(1e3),gauss)))
  valores2 <- replicate(1e3,mean(integral.montecarlo(runif(5e3),gauss)))
  valores3 <- replicate(1e3,mean(integral.montecarlo(runif(1e4),gauss)))
  valores4 <- replicate(1e3,mean(integral.montecarlo(runif(1e5),gauss)))
  
  alcance <- c(0.818,0.958)
  alcancey <- c(0,200)
  par(mfrow=c(2,2))
  
  hist(valores1,freq=F,border=F,xlim=c(alcance),ylim=alcancey,main="estimativa com N=1000",col="#999999")
  lines(density(valores1),col="blue",lty=2,lwd=1.5)
  abline(v=sqrt(pi)/2,col="#aa00aa",lwd=2)
  errop1 <- sd(valores1)/sqrt(1e3)
  text(x=0.93,y=200,labels=bquote(bar(x)==.(round(mean(valores1),digits=3))))
  text(x=0.93,y=175,labels=bquote(theta==.(round(sqrt(pi)/2,digits=2))))
  text(x=0.93,y=150,labels=paste("ep",round(errop1,3)))
  
  hist(valores2,freq=F,border=F,xlim=c(alcance),ylim=alcancey,main="estimativa com N=5000",col="#999999")
  lines(density(valores2),col="blue",lty=2,lwd=1.5)
  abline(v=sqrt(pi)/2,col="#aa00aa",lwd=2)
  errop2 <- sd(valores2)/sqrt(5e3)
  text(x=0.93,y=200,labels=bquote(bar(x)==.(round(mean(valores2),digits=3))))
  text(x=0.93,y=175,labels=bquote(theta==.(round(sqrt(pi)/2,digits=2))))
  text(x=0.93,y=150,labels=paste("ep",round(errop2,3)))
  
  hist(valores3,freq=F,border=F,xlim=c(alcance),ylim=alcancey,main="estimativa com N=10000",col="#999999")
  lines(density(valores3),col="blue",lty=2,lwd=1.5)
  abline(v=sqrt(pi)/2,col="#aa00aa",lwd=2)
  errop3 <- sd(valores3)/1e4
  text(x=0.93,y=200,labels=bquote(bar(x)==.(round(mean(valores3),digits=3))))
  text(x=0.93,y=175,labels=bquote(theta==.(round(sqrt(pi)/2,digits=2))))
  text(x=0.93,y=150,labels=paste("ep",round(errop3,3)))
  
  hist(valores4,freq=F,border=F,xlim=c(alcance),ylim=alcancey,main="estimativa com N=100000",col="#999999")
  lines(density(valores4),col="blue",lty=2,lwd=1.5)
  abline(v=sqrt(pi)/2,col="#aa00aa",lwd=2)
  errop4 <- sd(valores4)/1e5
  text(x=0.93,y=200,labels=bquote(bar(x)==.(round(mean(valores4),digits=3))))
  text(x=0.93,y=175,labels=bquote(theta==.(round(sqrt(pi)/2,digits=2))))
  text(x=0.93,y=150,labels=paste("ep",round(errop4,3)))
} 
grafico()

#outra parametrização da integral de 0 a infinito
#t=z/(1+x),x=t/(1-t), x=0 => t=0, x=\infty => t=1, dx=dt/((1-t)^2)

#outra outra parametrização de 0 a infinito.
#ela é = a mesma integral de 0 a 1, e depois de 1 a infinito.
#então faz o mesmo processo usando a transformação:v = 1/x